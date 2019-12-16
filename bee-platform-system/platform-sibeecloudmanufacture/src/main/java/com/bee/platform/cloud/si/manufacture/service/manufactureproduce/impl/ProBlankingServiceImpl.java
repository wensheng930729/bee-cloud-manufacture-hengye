package com.bee.platform.cloud.si.manufacture.service.manufactureproduce.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.dao.mapper.ProBlankingMapper;
import com.bee.platform.cloud.si.manufacture.dto.ProBlankingQueryDTO;
import com.bee.platform.cloud.si.manufacture.entity.*;
import com.bee.platform.cloud.si.manufacture.rq.ProBlankingRQ;
import com.bee.platform.cloud.si.manufacture.service.StorageService;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigDeviceService;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigPlcDeviceService;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigProductService;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigRawMaterialLossService;
import com.bee.platform.cloud.si.manufacture.service.manufactureproduce.*;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.PageUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import java.math.BigDecimal;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * <p>
 * 下料表 服务实现类
 * </p>
 *
 * @author huangxin123
 * @since 2019-09-25
 */
@Slf4j
@Service
public class ProBlankingServiceImpl extends ServiceImpl<ProBlankingMapper, ProBlanking> implements ProBlankingService {

    @Autowired
    private ProIngredientService ingredientService;

    @Autowired
    private ConfigDeviceService deviceService;

    @Autowired
    private StorageService storageService;

    @Autowired
    private ConfigPlcDeviceService configPlcDeviceService;

    @Autowired
    private ProMaterialBatchDetailService materialBatchDetailService;

    @Autowired
    private ProIngredientStatisticService ingredientStatisticService;

    @Autowired
    private ProIngredientDetailService ingredientDetailService;

    @Autowired
    private ConfigProductService productService;

    @Autowired
    private ProBlankingDetailService blankingDetailService;

    @Autowired
    private ConfigRawMaterialLossService rawMaterialLossService;

    /**
     * @descriptin 下料
     * @author xin.huang
     * @param rq
     * @param userInfo
     * @date 2019/9/25
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult blanking(ProBlankingRQ rq, AuthPlatformUserInfo userInfo) {
        ProBlanking proBlanking = BeanUtils.copyProperties(rq, ProBlanking.class);
        proBlanking.setCreateId(userInfo.getId())
                .setCompanyId(userInfo.getOrgId())
                .setFactoryId(userInfo.getFactoryId())
                .setCreateTime(new Date());
        //获取矿热炉信息
        ConfigDevice device = deviceService.selectOne(new EntityWrapper<>(new ConfigDevice()
                .setId(rq.getFurnaceId())
                .setStatus(Status.TRUE.getKey())));
        if (!ObjectUtils.isEmpty(device)) {
            proBlanking.setFurnaceName(device.getName());
        }
        //获取plc设备信息
        if (!ObjectUtils.isEmpty(rq.getPlcId())) {
            ConfigPlcDevice configPlcDevice = configPlcDeviceService.selectOne(new EntityWrapper<>(new ConfigPlcDevice()
                    .setId(rq.getPlcId())
                    .setStatus(Status.TRUE.getKey())));
            if (!ObjectUtils.isEmpty(configPlcDevice)) {
                proBlanking.setPlcName(configPlcDevice.getName());
            }
        }
        if (!this.insert(proBlanking)) {
            log.error("下料失败 类：{} 方法：{}", "ProBlankingServiceImpl", "blanking");
            return ResponseResult.buildResponseResult(ResCodeEnum.ADD_FAILED);
        }

        //扣减配料库存
        reduceStorage(rq, proBlanking.getId(), userInfo);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @descriptin 查询下料记录
     * @author xin.huang
     * @param rq
     * @param userInfo
     * @param pagination
     * @date 2019/9/25
     * @return
     */
    @Override
    public ResponseResult<List<ProBlankingQueryDTO>> findList(ProBlankingRQ rq, AuthPlatformUserInfo userInfo, Pagination pagination) {
        Wrapper<ProBlanking> wrapper = new EntityWrapper<ProBlanking>()
                .eq("status", Status.TRUE.getKey())
                .eq("company_id", userInfo.getOrgId())
                .eq("factory_id", userInfo.getFactoryId());
        if (!ObjectUtils.isEmpty(rq) && !ObjectUtils.isEmpty(rq.getMaterialName())) {
            wrapper.like("material_name", rq.getMaterialName());
        }
        wrapper.orderBy("create_time", false);
        List<ProBlanking> proIngredients = baseMapper.selectPage(pagination, wrapper);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,
                BeanUtils.assemble(ProBlankingQueryDTO.class, proIngredients),
                PageUtils.transToPage(pagination));
    }

    /**
     * @descriptin 扣减配料数量
     * @author xin.huang
     * @param
     * @date 2019/10/12
     * @return
     */
    private void reduceStorage(ProBlankingRQ rq, Long blankingId, AuthPlatformUserInfo userInfo) {
        List<ProMaterialBatchDetail> materialBatchDetails = materialBatchDetailService.selectList(new EntityWrapper<>(new ProMaterialBatchDetail()
                .setBatchId(rq.getBatchId()).setStatus(Status.TRUE.getKey())));
        if (ObjectUtils.isEmpty(materialBatchDetails) || ObjectUtils.isEmpty(rq.getTrains()) || rq.getTrains() < 1) {
            return;
        }
        //料批中所有产品数量之后
        BigDecimal materialTotalNum = materialBatchDetails.stream().map(ProMaterialBatchDetail::getNum)
                .reduce(BigDecimal::add).orElse(BigDecimal.ZERO);
        //此次下料的总重量
        BigDecimal totalNum = BigDecimal.ZERO;
        if (Objects.nonNull(materialTotalNum)) {
            totalNum = materialTotalNum.multiply(new BigDecimal(rq.getTrains()));
        }

        //主料数量
        BigDecimal mainNum = BigDecimal.ZERO;
        Map<String, BigDecimal> numMap = new HashMap<>();

        //存放产品id
        List<Integer> productIds = new ArrayList<>(materialBatchDetails.size());

        //下料明细
        List<ProBlankingDetail> details = new ArrayList<>(materialBatchDetails.size());
        if (totalNum.compareTo(BigDecimal.ZERO) == 1) {
            //下料车次
            BigDecimal trains = new BigDecimal(rq.getTrains());

            //下料时的重量
            for (ProMaterialBatchDetail detail : materialBatchDetails) {
                if (Objects.isNull(detail.getNum()) || detail.getNum().compareTo(BigDecimal.ZERO) == 0) {
                    continue;
                }
                productIds.add(detail.getProductId());

                //需要扣减的产品数量
                BigDecimal reduceNum = detail.getNum().multiply(trains);
                numMap.put(detail.getProductId() + "-" + detail.getProductSpecId(), reduceNum);

                //查询产品详情信息
                ConfigProduct configProduct = productService.selectOne(new EntityWrapper<>(new ConfigProduct()
                        .setId(detail.getProductId())
                        .setStatus(Status.TRUE.getKey())
                        .setDeleted(Status.FALSE.getKey())));
                if (!ObjectUtils.isEmpty(configProduct)) {
                    ProBlankingDetail blankingDetail = new ProBlankingDetail();
                    blankingDetail.setBlankingId(blankingId)
                            .setProductId(detail.getProductId())
                            .setProductName(detail.getProductName())
                            .setProductSpecId(detail.getProductSpecId())
                            .setProductSpecName(detail.getProductSpecName())
                            .setCreateTime(new Date())
                            .setNum(reduceNum);
                    if (configProduct.getCategoryId().equals(1)) {
                        //主料
                        blankingDetail.setType(0);
                        mainNum = mainNum.add(reduceNum);
                    } else if (configProduct.getCategoryId().equals(2)){
                        //辅料
                        blankingDetail.setType(1);
                    }
                    if (!ObjectUtils.isEmpty(blankingDetail.getType())) {
                        details.add(blankingDetail);
                    }
                }
            }
        }

        //添加下料明细
        if (!ObjectUtils.isEmpty(details)) {
            blankingDetailService.insertBatch(details);
        }

        List<ProIngredientStatistic> ingredientStatistics = ingredientStatisticService
                .selectList(new EntityWrapper<>(new ProIngredientStatistic()
                        .setBatchId(rq.getBatchId())
                        .setStatus(Status.TRUE.getKey())));
        if (!ObjectUtils.isEmpty(ingredientStatistics)) {
            Map<Integer, ConfigRawMaterialLoss> productLoseMap = new HashMap<>();
            if (ObjectUtils.isEmpty(rq.getPlcId())) {
                //获取产品损耗
                List<ConfigRawMaterialLoss> losses = rawMaterialLossService
                        .selectList(new EntityWrapper<ConfigRawMaterialLoss>()
                                .in("product_id", productIds)
                                .eq("deleted", Status.FALSE.getKey()));
                if (!ObjectUtils.isEmpty(losses)) {
                    productLoseMap.putAll(losses.stream().collect(Collectors
                            .toMap(ConfigRawMaterialLoss :: getProductId, Function.identity())));
                }
            }

            ingredientStatistics.forEach(detail -> {
                if (numMap.containsKey(detail.getProductId() + "-" + detail.getProductSpecId())) {
                    detail.setNum(detail.getNum().subtract(numMap
                            .get(detail.getProductId() + "-" + detail.getProductSpecId())));
                }
            });

            ingredientStatisticService.updateBatchById(ingredientStatistics);
        }

        //更新下料表中的主料数量,总重量
        this.updateById(new ProBlanking().setId(blankingId).setMainNum(mainNum).setNum(totalNum));

    }
}
