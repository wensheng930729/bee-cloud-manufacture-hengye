package com.bee.platform.cloud.si.manufacture.service.manufactureproduce.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.dao.mapper.ProIngredientMapper;
import com.bee.platform.cloud.si.manufacture.dao.mapper.StorageInventoryMapper;
import com.bee.platform.cloud.si.manufacture.dto.ProIngredientDTO;
import com.bee.platform.cloud.si.manufacture.dto.ProIngredientDetailDTO;
import com.bee.platform.cloud.si.manufacture.dto.ProIngredientStatisticDTO;
import com.bee.platform.cloud.si.manufacture.dto.ProductionOutStorageDetailDTO;
import com.bee.platform.cloud.si.manufacture.entity.*;
import com.bee.platform.cloud.si.manufacture.rq.ProIngredientDetailRQ;
import com.bee.platform.cloud.si.manufacture.rq.ProIngredientRQ;
import com.bee.platform.cloud.si.manufacture.service.PlcRealDataService;
import com.bee.platform.cloud.si.manufacture.service.StorageService;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigProductService;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigRawMaterialLossService;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigRepositoryService;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigWeighDeviceService;
import com.bee.platform.cloud.si.manufacture.service.manufactureproduce.*;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.BigDecimalUtils;
import com.bee.platform.common.utils.PageUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.math.BigDecimal;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * <p>
 * 配料主表 服务实现类
 * </p>
 *
 * @author huangxin123
 * @since 2019-09-25
 */
@Slf4j
@Service
public class ProIngredientServiceImpl extends ServiceImpl<ProIngredientMapper, ProIngredient> implements ProIngredientService {

    @Autowired
    private ProIngredientDetailService ingredientDetailService;

    @Autowired
    private StorageInventoryMapper storageInventoryMapper;

    @Autowired
    private StorageService storageService;

    @Autowired
    private ConfigProductService productService;

    @Autowired
    private ConfigRepositoryService repositoryService;

    @Autowired
    private ConfigWeighDeviceService weighDeviceService;

    @Autowired
    private ProMaterialBatchService materialBatchService;

    @Autowired
    private ProIngredientStatisticService ingredientStatisticService;

    @Autowired
    private ProIngredientMapper proIngredientMapper;

    @Autowired
    private PlcRealDataService plcRealDataService;

    @Autowired
    private ProMaterialBatchDetailService materialBatchDetailService;

    @Autowired
    private ConfigRawMaterialLossService rawMaterialLossService;

    @Autowired
    private ProIngredientRecordService ingredientRecordService;

    /**
     * @descriptin 新增配料
     * @author xin.huang
     * @param rq
     * @param userInfo
     * @date 2019/9/25
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult addIngredient(ProIngredientRQ rq, AuthPlatformUserInfo userInfo) {
        //获取料批相关信息
        EntityWrapper<ProMaterialBatch> wrapper = new EntityWrapper<>(new ProMaterialBatch()
                .setStatus(Status.TRUE.getKey()).setActive(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(rq.getPlcId())) {
            wrapper.eq("id", rq.getBatchId());
        } else {
            wrapper.eq("plc_id", rq.getPlcId());
        }
        ProMaterialBatch proMaterialBatch = materialBatchService.selectOne(wrapper);
        if (ObjectUtils.isEmpty(proMaterialBatch)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_DATA);
        }

        ProIngredient proIngredient = BeanUtils.copyProperties(rq, ProIngredient.class);
        proIngredient.setCreateTime(new Date())
                .setCreateId(userInfo.getId())
                .setCompanyId(userInfo.getOrgId())
                .setFactoryId(userInfo.getFactoryId())
                .setType(Status.FALSE.getKey())
                .setMaterialName(proMaterialBatch.getMaterialName());

        //获取plc设备信息
        if (!ObjectUtils.isEmpty(proMaterialBatch.getPlcId())) {
            proIngredient.setPlcId(proMaterialBatch.getPlcId())
                    .setPlcName(proMaterialBatch.getPlcName())
                    .setType(Status.TRUE.getKey());
        }

        //新增配料基本信息
        if (!this.insert(proIngredient)) {
            log.error("添加配料基本信息失败 类：{} 方法：{}", "ProIngredientServiceImpl", "addIngredient");
            return ResponseResult.buildResponseResult(ResCodeEnum.ADD_FAILED);
        }

        //新增配料明细信息
        //若是定时保存，则根据plcId获取产品相关信息
        if (!ObjectUtils.isEmpty(rq.getPlcId()) && !ObjectUtils.isEmpty(rq.getTimeSave())
                && rq.getTimeSave().equals(Status.TRUE.getKey())) {
            List<ProIngredientRecord> records = ingredientRecordService
                    .selectList(new EntityWrapper<>(new ProIngredientRecord()
                            .setBatchId(proMaterialBatch.getId())
                            .setStatus(Status.TRUE.getKey())));
            if (!ObjectUtils.isEmpty(records)) {
                rq.setIngredientDetailList(BeanUtils.assemble(ProIngredientDetailRQ.class, records));
            }
        }

        //存放带规格的产品数量
        Map<String, BigDecimal> productNumMap = new HashMap<String, BigDecimal>();
        if (!CollectionUtils.isEmpty(rq.getIngredientDetailList())) {
            List<ProIngredientDetail> ingredientDetails = BeanUtils.assemble(ProIngredientDetail.class, rq.getIngredientDetailList());
            //存放产品id
            List<Integer> productIds = new ArrayList<>(ingredientDetails.size());

            ingredientDetails.forEach(detail -> {
                productIds.add(detail.getProductId());
                productNumMap.put(detail.getProductId() + "-" + detail.getProductSpecId(), detail.getNum());
                detail.setIngredientId(proIngredient.getId()).setCreateTime(new Date());
                //获取产品明细信息
                ConfigProduct product = productService.selectOne(new EntityWrapper<>(new ConfigProduct()
                        .setId(detail.getProductId())
                        .setStatus(Status.TRUE.getKey())));
                if (!ObjectUtils.isEmpty(product)) {
                    detail.setProductName(product.getName()).setUnit(product.getUnitValue());
                }

                //获取产品所在仓库信息
                ConfigRepository repository = repositoryService.selectOne(new EntityWrapper<>(new ConfigRepository()
                        .setId(detail.getWarehouseId())
                        .setStatus(Status.TRUE.getKey())));
                if (!ObjectUtils.isEmpty(repository)) {
                    detail.setWarehouseName(repository.getName());
                }

                //获取称重设备信息
                if (!ObjectUtils.isEmpty(detail.getWeighDeviceId())) {
                    ConfigWeighDevice weighDevice = weighDeviceService.selectOne(new EntityWrapper<>(new ConfigWeighDevice()
                            .setId(detail.getWarehouseId())
                            .setStatus(Status.TRUE.getKey())));
                    if (!ObjectUtils.isEmpty(weighDevice)) {
                        detail.setWeighDeviceName(weighDevice.getName());
                    }

                }
            });

            if (!ingredientDetailService.insertBatch(ingredientDetails)) {
                log.error("添加配料明细信息失败 类：{} 方法：{}", "ProIngredientServiceImpl", "addIngredient");
                return ResponseResult.buildResponseResult(ResCodeEnum.ADD_FAILED);
            }

            //若不存在plc，则添加或更新配料统计信息
            if (ObjectUtils.isEmpty(rq.getPlcId())) {

                //更新当前料批关联的配料统计明细中的产品数量
                updateIngredientStatic(rq, proIngredient, productNumMap, ingredientDetails);
            } else {
                //清空配料统计表中的相关产品数量
                ingredientRecordService.update(new ProIngredientRecord().setNum(BigDecimal.ZERO),
                        new EntityWrapper<>(new ProIngredientRecord()
                                .setBatchId(proMaterialBatch.getId())
                                .setStatus(Status.TRUE.getKey())));
            }

            //扣减库存
            deductInventory(ingredientDetails, productIds, userInfo);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @descriptin 更新当前料批相关的统计配料明细中的产品数量信息
     * @author xin.huang
     * @param
     * @date 2019/10/15
     * @return
     */
    private void updateIngredientStatic(ProIngredientRQ rq, ProIngredient proIngredient,
                                        Map<String, BigDecimal> productNumMap,
                                        List<ProIngredientDetail> ingredientDetails) {
        //查询当前料批相关的统计配料明细信息
        List<ProIngredientStatistic> statistics = ingredientStatisticService
                .selectList(new EntityWrapper<>(new ProIngredientStatistic()
                        .setBatchId(rq.getBatchId())
                        .setStatus(Status.TRUE.getKey())));
        //若已配料，则更新产品数量
        if (!ObjectUtils.isEmpty(statistics)) {
            statistics.forEach(detail -> {
                if (productNumMap.containsKey(detail.getProductId() + "-" + detail.getProductSpecId())) {
                    detail.setNum(detail.getNum()
                            .add(productNumMap.get(detail.getProductId() + "-" + detail.getProductSpecId())));
                }
            });
            ingredientStatisticService.updateBatchById(statistics);
        } else {
            List<ProIngredientStatistic> ingredientStatistics = BeanUtils.assemble(ProIngredientStatistic.class, ingredientDetails);
            ingredientStatistics.forEach(detail -> {
                detail.setPlcId(proIngredient.getPlcId())
                        .setPlcName(proIngredient.getPlcName())
                        .setBatchId(proIngredient.getBatchId())
                        .setMaterialName(proIngredient.getMaterialName())
                        .setFinishProductId(rq.getFinishProductId())
                        .setCreateTime(new Date());
            });
            ingredientStatisticService.insertBatch(ingredientStatistics);
        }
    }

    /**
     * @descriptin 查询配料列表
     * @author xin.huang
     * @param rq
     * @param userInfo
     * @date 2019/9/25
     * @return
     */
    @Override
    public ResponseResult<List<ProIngredientDTO>> findList(ProIngredientRQ rq, AuthPlatformUserInfo userInfo, Pagination pagination) {
        Wrapper<ProIngredient> wrapper = new EntityWrapper<ProIngredient>()
                .eq("status", Status.TRUE.getKey())
                .eq("company_id", userInfo.getOrgId())
                .eq("factory_id", userInfo.getFactoryId());
        if (!ObjectUtils.isEmpty(rq)) {
            if (!ObjectUtils.isEmpty(rq.getMaterialName())) {
                wrapper.like("material_name", rq.getMaterialName());
            }
            if (!ObjectUtils.isEmpty(rq.getType())) {
                wrapper.eq("type", rq.getType());
            }
        }
        wrapper.orderBy("create_time", false);
        List<ProIngredient> proIngredients = baseMapper.selectPage(pagination, wrapper);
        if (CollectionUtils.isEmpty(proIngredients)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, Collections.EMPTY_LIST , PageUtils.transToPage(pagination));
        }
        List<ProIngredientDTO> proIngredientDTOS = BeanUtils.assemble(ProIngredientDTO.class, proIngredients);
        proIngredientDTOS.forEach(detail -> {
            List<ProIngredientDetail> ingredientDetails = ingredientDetailService.selectList(new EntityWrapper<>(new ProIngredientDetail()
                    .setIngredientId(detail.getId())
                    .setStatus(Status.TRUE.getKey())));
            detail.setIngredientDetailList(BeanUtils.assemble(ProIngredientDetailDTO.class, ingredientDetails));
        });
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, proIngredientDTOS, PageUtils.transToPage(pagination));
    }

    /**
     * @descriptin 查询未下料列表
     * @author xin.huang
     * @param userInfo
     * @date 2019/10/6
     * @return
     */

    @Override
    public ResponseResult<List<ProIngredientStatisticDTO>> findBlankingList(AuthPlatformUserInfo userInfo, Integer havePlc) {
        List<ProIngredientStatisticDTO> proIngredientDTOS = new ArrayList<ProIngredientStatisticDTO>();
        Wrapper<ProMaterialBatch> wrapper = new EntityWrapper<ProMaterialBatch>()
                .eq("status", Status.TRUE.getKey())
                .eq("company_id", userInfo.getOrgId())
                .eq("factory_id", userInfo.getFactoryId())
                .eq("active", Status.TRUE.getKey());
        if (havePlc.equals(Status.FALSE.getKey())) {
            //不存在plc
            wrapper.isNull("plc_id");
        } else {
            wrapper.isNotNull("plc_id");
        }
        List<ProMaterialBatch> materialBatches = materialBatchService.selectList(wrapper);
        if (!CollectionUtils.isEmpty(materialBatches)) {
            List<Long> batchIds = new ArrayList<>(materialBatches.size());
            for (ProMaterialBatch batch : materialBatches) {
                batchIds.add(batch.getId());
            }
            List<ProIngredientStatistic> ingredientStatistics = ingredientStatisticService
                    .selectList(new EntityWrapper<ProIngredientStatistic>()
                            .in("batch_id", batchIds)
                            .orderBy("create_time", false));
            if (!ObjectUtils.isEmpty(ingredientStatistics)) {
                Map<Long, List<ProIngredientStatistic>> map = ingredientStatistics.stream().collect(Collectors.groupingBy(ProIngredientStatistic :: getBatchId));
                for (Map.Entry<Long, List<ProIngredientStatistic>> entry : map.entrySet()) {
                    ProIngredientStatisticDTO statisticDTO = new ProIngredientStatisticDTO();
                    List<ProIngredientStatistic> statistics = entry.getValue();
                    Integer plcId = statistics.get(0).getPlcId();
                    statisticDTO.setBatchId(statistics.get(0).getBatchId())
                            .setMaterialName(statistics.get(0).getMaterialName())
                            .setPlcId(plcId)
                            .setPlcName(statistics.get(0).getPlcName())
                            .setFinishProductId(statistics.get(0).getFinishProductId());
                    statisticDTO.setIngredientDetailList(BeanUtils.assemble(ProIngredientDetailDTO.class, statistics));

                    //获取plc下料时间
                    if (!ObjectUtils.isEmpty(plcId)) {
                        PlcRealData plcRealData = plcRealDataService.selectOne(new EntityWrapper<>(new PlcRealData()
                                .setPlcId(plcId)
                                .setStatus(Status.TRUE.getKey()))
                                .orderBy("time", false)
                                .last("limit 1"));
                        if (!ObjectUtils.isEmpty(plcRealData)) {
                            statisticDTO.setBlankingTime(plcRealData.getTime());
                        }
                    }
                    proIngredientDTOS.add(statisticDTO);
                }
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, proIngredientDTOS);
    }

    /**
     * @notes: 根据plc查询最后一次保存料批的时间
     * @Author: junyang.li
     * @Date: 15:19 2019/10/12
     * @param plcId :
     * @return: java.util.Date
     */
    @Override
    public Date getLastPreserve(int plcId) {
        return proIngredientMapper.getLastPreserve(plcId);
    }

    /**
     * @descriptin 扣减产品库存
     * @author xin.huang
     * @param productIds
     * @param userInfo
     * @date 2019/9/25
     * @return
     */
    private synchronized void deductInventory (List<ProIngredientDetail> ingredientDetails,
                                               List<Integer> productIds,
                                               AuthPlatformUserInfo userInfo) {
            //获取产品损耗
            Map<Integer, ConfigRawMaterialLoss> productLoseMap = new HashMap<>();
            List<ConfigRawMaterialLoss> losses = rawMaterialLossService
                    .selectList(new EntityWrapper<ConfigRawMaterialLoss>()
                            .in("product_id", productIds)
                            .eq("deleted", Status.FALSE.getKey()));
            if (!ObjectUtils.isEmpty(losses)) {
                productLoseMap.putAll(losses.stream().collect(Collectors
                        .toMap(ConfigRawMaterialLoss :: getProductId, Function.identity())));
            }

            for (ProIngredientDetail detail : ingredientDetails) {
                ProductionOutStorageDetailDTO storage = new ProductionOutStorageDetailDTO();
                storage.setProductId(detail.getProductId())
                        .setProductName(detail.getProductName())
                        .setProductUnit(detail.getUnit())
                        .setProductSpecId(detail.getProductSpecId())
                        .setProductSpecName(detail.getProductSpecName())
                        .setStorageId(detail.getWarehouseId())
                        .setStorageName(detail.getWarehouseName());
                BigDecimal num = detail.getNum();
                if (productLoseMap.containsKey(detail.getProductId())) {
                    BigDecimal loss = productLoseMap.get(detail.getProductId()).getLoss();
                    if (Objects.nonNull(num) && Objects.nonNull(loss)) {
                        num = num.multiply(new BigDecimal("1").add(loss));
                    }
                }
                storage.setProductNumber(num);
                storageService.saveProductionOutStorage(storage, userInfo);
            }

    }
}
