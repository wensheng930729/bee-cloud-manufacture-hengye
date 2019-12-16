package com.bee.platform.cloud.si.manufacture.service.manufacturebuy.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.Page;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.dao.mapper.*;
import com.bee.platform.cloud.si.manufacture.dto.*;
import com.bee.platform.cloud.si.manufacture.entity.*;
import com.bee.platform.cloud.si.manufacture.enums.EnumSampleRelation;
import com.bee.platform.cloud.si.manufacture.enums.EnumWeightMachine;
import com.bee.platform.cloud.si.manufacture.rq.*;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigProductService;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigProductSpecService;
import com.bee.platform.cloud.si.manufacture.service.manufacturebuy.BuyTransportSectionService;
import com.bee.platform.cloud.si.manufacture.service.manufacturebuy.BuyWeightMachineService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.EnumGenerateIdModule;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.service.GenerateIdService;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.DateUtils;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.math.BigDecimal;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.*;


/**
 * @author chenxm66777123
 * @version 1.0.0
 * @Description 地磅数据信息 服务实现类
 * @Date 2019/9/23 13:49
 */
@Slf4j
@Service
public class BuyWeightMachineServiceImpl extends ServiceImpl<BuyWeightMachineMapper, BuyWeightMachine> implements BuyWeightMachineService {

    @Autowired
    private BuyWeightMachineMapper buyWeightMachineMapper;

    @Autowired
    private BuyWeightMachineFileMapper buyWeightMachineFileMapper;

    @Autowired
    private WeightMachineDataMapper weightMachineDataMapper;

    @Autowired
    private GenerateIdService generateIdService;

    @Autowired
    private BuyContractBasicMapper buyContractBasicMapper;

    @Autowired
    private ConfigProductService configProductService;

    @Autowired
    private ConfigProductSpecService configProductSpecService;

    @Autowired
    private BuyTransportSectionService buyTransportSectionService;

    @Autowired
    private BuyGoodsPendingStorageMapper buyGoodsPendingStorageMapper;
    /**
     * @Description 保存地磅数据信息
     * @author chenxm66777123
     * @Date 2019/9/24 9:23
     * @version 1.0.0
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<String> saveBuyWeightMachine(WeightMachineRq weightMachineRq, AuthPlatformUserInfo userInfo, Integer dataSource) {
        //判断是否传过来榜单id
        BuyWeightMachine buyWeightMachine;

        //提前处理附件信息
        List<BuyWeightMachineFile> buyWeightMachineFileList = new ArrayList<>();
        if (!CollectionUtils.isEmpty(weightMachineRq.getFiles())) {
            buyWeightMachineFileList = BeanUtils.assemble(BuyWeightMachineFile.class, weightMachineRq.getFiles());
            buyWeightMachineFileList.stream().forEach(obj -> {
                obj.setMachineId(weightMachineRq.getMachineId());
                obj.setStatus(Status.TRUE.getKey());
                obj.setCreateId(userInfo.getId());
                obj.setCreator(userInfo.getName());
                obj.setCreateTime(new Date());
            });
        }

        //保存操作，如果没有磅单编号，则进行保存操作
        if (StringUtils.isBlank(weightMachineRq.getMachineId())) {
            //复制传入参数
            buyWeightMachine = BeanUtils.copyProperties(weightMachineRq, BuyWeightMachine.class);

            //生成id
            String generateBusinessId = generateIdService.generateBusinessId(EnumGenerateIdModule.Module.BUY_WEIGHT_MACHINE.getKey());
            buyWeightMachine.setMachineId(generateBusinessId);

            //企业信息
            if (ObjectUtils.isEmpty(buyWeightMachine.getEnterpriseId())) {
                buyWeightMachine.setEnterpriseId(userInfo.getOrgId());
            }
            if (ObjectUtils.isEmpty(buyWeightMachine.getFactoryId())) {
                buyWeightMachine.setFactoryId(userInfo.getFactoryId());
            }
            BigDecimal inFactoryWeight = BigDecimal.ZERO;
            if (!ObjectUtils.isEmpty(weightMachineRq.getInFactoryWeight())) {
                //千克转换为吨
                inFactoryWeight = weightMachineRq.getInFactoryWeight();
                buyWeightMachine.setInFactoryWeight(inFactoryWeight);
                Integer weightType = ObjectUtils.isEmpty(weightMachineRq.getInFactoryWeightByManual()) ? 0 : weightMachineRq.getInFactoryWeightByManual();
                //是否手动修改  0 自动 1手动
                buyWeightMachine.setInFactoryWeightByManual(weightType);
                //是否确认称重 0 未确认 1 确认
                buyWeightMachine.setInFactoryWeightIsConfirm(Status.TRUE.getKey());
                //称重时间
                buyWeightMachine.setWeighingTime(new Date());
                //司磅员
                buyWeightMachine.setWeightMan(userInfo.getName());
            }
            BigDecimal outFactoryWeight = BigDecimal.ZERO;

            if (!ObjectUtils.isEmpty(weightMachineRq.getOutFactoryWeight())) {
                //千克转换为吨
                outFactoryWeight = weightMachineRq.getOutFactoryWeight();
                buyWeightMachine.setOutFactoryWeight(outFactoryWeight);
                Integer weightType = ObjectUtils.isEmpty(weightMachineRq.getOutFactoryWeightByManual()) ? 0 : weightMachineRq.getOutFactoryWeightByManual();
                //是否手动修改  0 自动 1手动
                buyWeightMachine.setOutFactoryWeightByManual(weightType);
                //是否确认称重 0 未确认 1 确认
                buyWeightMachine.setOutFactoryWeightIsConfirm(Status.TRUE.getKey());
            }

            if (EnumWeightMachine.DataSource.ADD.getValue().equals(dataSource)
                    && inFactoryWeight.compareTo(BigDecimal.ZERO) <= 0) {
                return ResponseResult.buildResponseResult(ResCodeEnum.IN_WEIGHT_ZERO);
            }

            if (EnumWeightMachine.DataSource.ADD.getValue().equals(dataSource)
                   ) {
                //取样推送状态 0未推送 1已推送
                buyWeightMachine.setSamplePushStatus(Status.TRUE.getKey());
                buyWeightMachine.setSamplePushTime(new Date());
            }

            //新增数据来源
            buyWeightMachine.setDataSource(dataSource);
            //修改净重
            buyWeightMachine.setNetWeight(BigDecimal.ZERO);

            //实际到厂时间取确认榜单的时间
            buyWeightMachine.setArrivalTime(new Date());

            //创建人信息
            buyWeightMachine.setCreateId(userInfo.getId());
            buyWeightMachine.setCreator(userInfo.getName());
            buyWeightMachine.setCreateTime(new Date());

            buyWeightMachine.setModifyId(userInfo.getId());
            buyWeightMachine.setModifier(userInfo.getName());
            buyWeightMachine.setModifyTime(new Date());

            buyWeightMachine.setReceivingCompany(userInfo.getOrg_name());

            //地磅处理状态信息
            buyWeightMachine.setStatus(Status.TRUE.getKey());
            buyWeightMachine.setIsWeight(EnumWeightMachine.IsWeight.NO.getValue());
            buyWeightMachine.setSampleStatus(EnumSampleRelation.SampleResults.NO.getKey());
            buyWeightMachineMapper.insert(buyWeightMachine);

            if (!CollectionUtils.isEmpty(buyWeightMachineFileList)) {
                buyWeightMachineFileMapper.batchSaveFile(buyWeightMachineFileList);
            }
            return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_SUCCESS, buyWeightMachine.getMachineId());
        }

        //进行更新操作
        //未上传磅单信息不能确认磅单
        buyWeightMachine = buyWeightMachineMapper.selectOne(new BuyWeightMachine().setMachineId(weightMachineRq.getMachineId()).setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(buyWeightMachine)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_FAIL_AND_NOT_FOUND_DATA);
        }

        BuyWeightMachine updateBuyWeightMachine = BeanUtils.copyProperties(weightMachineRq, BuyWeightMachine.class);

        //地磅处理信息处理
        updateBuyWeightMachine.setStatus(buyWeightMachine.getStatus());
        //是否称重，确认后置位已称重
        updateBuyWeightMachine.setIsWeight(EnumWeightMachine.IsWeight.YES.getValue());
        //通知取样状态
        updateBuyWeightMachine.setSamplePushStatus(Status.TRUE.getKey());
        //通知取样时间
        updateBuyWeightMachine.setSamplePushTime(new Date());
        //是否需要取样
        updateBuyWeightMachine.setSampleStatus(buyWeightMachine.getSampleStatus());
        //是否化验
        updateBuyWeightMachine.setAssayResult(buyWeightMachine.getAssayResult());
        //状态
        updateBuyWeightMachine.setStatus(buyWeightMachine.getStatus());

        //司磅员
        updateBuyWeightMachine.setWeightMan(userInfo.getName());

        //创建者信息
        updateBuyWeightMachine.setCreateId(buyWeightMachine.getCreateId());
        updateBuyWeightMachine.setCreator(buyWeightMachine.getCreator());
        updateBuyWeightMachine.setCreateTime(buyWeightMachine.getCreateTime());
        updateBuyWeightMachine.setModifyId(userInfo.getId());
        updateBuyWeightMachine.setModifier(userInfo.getName());
        updateBuyWeightMachine.setModifyTime(new Date());
        buyWeightMachineMapper.updateAllColumnById(updateBuyWeightMachine);

        //保存磅单附件信息
        buyWeightMachineFileMapper.batchSaveFile(buyWeightMachineFileList);


        return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_SUCCESS, buyWeightMachine.getMachineId());
    }


    /**
     * @Description 根据企业查询称重信息
     * @author chenxm66777123
     * @Date 2019/9/24 11:33
     * @version 1.0.0
     */
    @Override
    public List<WeightMachineDTO> getBuyWeightMachineList(Integer orgId, Pagination pagination) {
        List<BuyWeightMachine> buyWeightMachines = buyWeightMachineMapper.
                selectPage(pagination, new EntityWrapper<BuyWeightMachine>()
                        .where("status = 1 and enterprise_id = {0} and is_weight = {1}", orgId, Status.FALSE.getKey()).orderBy("create_time", false));
        List<WeightMachineDTO> list = BeanUtils.assemble(WeightMachineDTO.class, buyWeightMachines);
        if (!CollectionUtils.isEmpty(list)) {
            list.stream().forEach(obj -> {
                List<BuyWeightMachineFile> buyWeightMachineFiles = buyWeightMachineFileMapper.selectList(
                        new EntityWrapper<BuyWeightMachineFile>().where("status = 1 and machine_id = {0}", obj.getMachineId()));
                obj.setFiles(BeanUtils.assemble(CommonFileRq.class, buyWeightMachineFiles));
                //处理单位换算问题 吨转化为kg
                //进厂重量
                obj.setInFactoryWeight(tonTokg(obj.getInFactoryWeight()));
                //出厂重量
                obj.setOutFactoryWeight(tonTokg(obj.getOutFactoryWeight()));
                //净厂重量
                obj.setNetWeight(tonTokg(obj.getNetWeight()));
            });
        }
        return list;
    }

    /**
     * 查询过磅单数据信息
     *
     * @param rq
     * @param userInfo
     * @param pagination
     * @return
     */
    @Override
    public List<WeighingListDTO> getWeighListInfo(WeighingListRq rq, AuthPlatformUserInfo userInfo, Pagination pagination) {

        Map<String, Object> params = new HashMap<>(10);
        //拼接查询参数
        params.put("orgId", userInfo.getOrgId());
        if (!StringUtils.isEmpty(rq.getContractNum())) {
            params.put("contractNum", rq.getContractNum());
        }
        if (!StringUtils.isEmpty(rq.getTrainNumber())) {
            params.put("trainNumber", rq.getTrainNumber());
        }
        params.put("weighingType", rq.getWeighingType());
        if (!StringUtils.isEmpty(rq.getStartTime())) {
            params.put("startTime", rq.getStartTime() + DateUtils.TIME_SUFFIX);
        }
        if (!StringUtils.isEmpty(rq.getEndTime())) {
            params.put("endTime", rq.getEndTime() + DateUtils.TIME_END);
        }

        return buyWeightMachineMapper.getWeighListInfo(params, pagination);
    }


    /**
     * @Description 根据企业查询称重信息（总览）
     * @author chenxm66777123
     * @Date 2019/9/24 11:33
     * @version 1.0.0
     */
    @Override
    public List<WeightMachineTotalDTO> getWeightMachineListTotal(Integer orgId, Pagination pagination) {
        List<WeightMachineTotalDTO> list = buyWeightMachineMapper.getWeightMachineListTotal(orgId, pagination);
        //根据合同编号去获取信息
        list.stream().forEach(obj -> {
            if (ObjectUtils.isEmpty(obj.getAlreadyWeightAmount())) {
                obj.setAlreadyWeightAmount(BigDecimal.ZERO);
            }
            obj.setAlreadyWeightAmount(tonTokg(obj.getAlreadyWeightAmount()));

            if (!StringUtils.isEmpty(obj.getContractNum())) {
                BuyContractBasic buyContractBasic = buyContractBasicMapper.selectOne(
                        new BuyContractBasic().setContractNum(obj.getContractNum()).setStatus(Status.TRUE.getKey())
                );
                if (!ObjectUtils.isEmpty(buyContractBasic)) {
                    obj.setContractWeight(tonTokg(buyContractBasic.getQuantity()));
                } else {
                    obj.setContractWeight(BigDecimal.ZERO);
                }
            } else {
                obj.setContractWeight(BigDecimal.ZERO);
            }
        });
        return list;
    }

    @Override
    public List<WeightMachineDTO> getWeightMachineListDetails(Integer orgId, String contractNum, Pagination pagination) {
        List<BuyWeightMachine> buyWeightMachines = buyWeightMachineMapper.
                selectPage(pagination, new EntityWrapper<BuyWeightMachine>()
                        .where("status = 1 and enterprise_id = {0} and is_weight = {1} and contract_num = {2}"
                                , orgId, Status.TRUE.getKey(), contractNum).orderBy("create_time", false));
        List<WeightMachineDTO> list = BeanUtils.assemble(WeightMachineDTO.class, buyWeightMachines);
        if (!CollectionUtils.isEmpty(list)) {
            list.stream().forEach(obj -> {
                List<BuyWeightMachineFile> buyWeightMachineFiles = buyWeightMachineFileMapper.selectList(
                        new EntityWrapper<BuyWeightMachineFile>().where("status = 1 and machine_id = {0}", obj.getMachineId()));
                obj.setFiles(BeanUtils.assemble(CommonFileRq.class, buyWeightMachineFiles));

                //处理单位换算问题 吨转化为kg
                //出厂重量
                obj.setOutFactoryWeight(tonTokg(obj.getOutFactoryWeight()));
                //进厂重量
                obj.setInFactoryWeight(tonTokg(obj.getInFactoryWeight()));
                //净厂重量
                obj.setNetWeight(tonTokg(obj.getNetWeight()));
            });
        }
        return list;
    }

    /**
     * 多车一样界面根据产品id查询未完成合同的磅单号下拉列表
     */
    @Override
    public ResponseResult<List<BuyWeightMachineBoxDTO>> getWeitghtIdByProductId(Integer productId, AuthPlatformUserInfo userInfo) {
        HashMap<String, Object> map = new HashMap<>();
        map.put("orgId", userInfo.getOrgId());
        map.put("factoryId", userInfo.getFactoryId());
        map.put("productId", productId);
        List<BuyWeightMachineBoxDTO> resultList = buyWeightMachineMapper.getWeitghtIdByProductId(map);
        if (CollectionUtils.isEmpty(resultList)) {
            log.info("未查询到磅单下拉列表数据，方法：{}", "getWeitghtIdByProductId");
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new ArrayList<>(0));
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, resultList);
    }

    /**
     * 根据地磅id修改车次的折扣单价
     *
     * @param machineId
     * @param unitPrice
     * @param userInfo
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateDiscountUnitPrice(String machineId, BigDecimal unitPrice, AuthPlatformUserInfo userInfo) {

        //查询地磅信息
        BuyWeightMachine buyWeightMachine = buyWeightMachineMapper.selectOne(new BuyWeightMachine().setMachineId(machineId).setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(buyWeightMachine)) {
            log.error("根据地磅id查询不到地磅数据！");
            throw new BusinessException(ResCodeEnum.ERROR_NOT_FOUND, ExceptionMessageEnum.SYSTEM_NOT_FOUND);
        }

        buyWeightMachine.setDiscountUnitPrice(unitPrice);
        buyWeightMachine.setModifyId(userInfo.getId());
        buyWeightMachine.setModifier(userInfo.getName());
        buyWeightMachine.setModifyTime(new Date());
        if (!this.updateById(buyWeightMachine)) {
            log.error("更新地磅中折扣单价失败！");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERROR_SYSTEM);
        }
    }

    /**
     * @Description 保存称重信息
     * @author chenxm66777123
     * @Date 2019/9/28 14:15
     * @version 1.0.0
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<String> buyConfirmWeight(ConfirmWeightMachineRq confirmWeightMachineRq, AuthPlatformUserInfo userInfo) {
        //查询地磅信息
        BuyWeightMachine buyWeightMachine = buyWeightMachineMapper.selectOne(new BuyWeightMachine().setMachineId(confirmWeightMachineRq.getMachineId()).setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(buyWeightMachine)) {
            log.error("根据地磅id查询不到地磅数据！");
            throw new BusinessException(ResCodeEnum.ERROR_NOT_FOUND, ExceptionMessageEnum.SYSTEM_NOT_FOUND);
        }
        Integer type = confirmWeightMachineRq.getType();
        //进厂称重
        if (EnumWeightMachine.ConfirmWeightType.IN_FACTORY_WEIGHT.getValue().equals(type)) {

            //千克转换为吨
            BigDecimal inFactoryWeight = confirmWeightMachineRq.getInFactoryWeight();

            buyWeightMachine.setInFactoryWeight(inFactoryWeight);
            //是否手动修改  0 自动 1手动 2 手动 （设备异常）
            buyWeightMachine.setInFactoryWeightByManual(confirmWeightMachineRq.getInFactoryWeightByManual());
            //是否确认称重 0 未确认 1 确认
            buyWeightMachine.setInFactoryWeightIsConfirm(Status.TRUE.getKey());

            //修改净重
            BigDecimal netWeight = inFactoryWeight.subtract(buyWeightMachine.getOutFactoryWeight())
                    .subtract(bigDecimalIsNull(buyWeightMachine.getDeductWeight()));
            buyWeightMachine.setNetWeight(netWeight);

            //司磅员
            buyWeightMachine.setWeightMan(userInfo.getName());

            //修改者信息
            buyWeightMachine.setModifyId(userInfo.getId());
            buyWeightMachine.setModifier(userInfo.getName());
            buyWeightMachine.setModifyTime(new Date());
            //称重时间
            buyWeightMachine.setWeighingTime(new Date());

            buyWeightMachineMapper.updateById(buyWeightMachine);
            return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_SUCCESS);
        }
        //出厂称重
        if (EnumWeightMachine.ConfirmWeightType.OUT_FACTORY_WEIGHT.getValue().equals(type)) {
            //千克转换为吨
            BigDecimal outFactoryWeight = confirmWeightMachineRq.getOutFactoryWeight();

            buyWeightMachine.setOutFactoryWeight(outFactoryWeight);
            //是否手动修改  0 自动 1手动 2 手动 （设备异常）
            buyWeightMachine.setOutFactoryWeightByManual(confirmWeightMachineRq.getOutFactoryWeightByManual());
            //是否确认称重 0 未确认 1 确认
            buyWeightMachine.setOutFactoryWeightIsConfirm(Status.TRUE.getKey());
            //修改净重
            BigDecimal netWeight = buyWeightMachine.getInFactoryWeight().subtract(outFactoryWeight)
                    .subtract(bigDecimalIsNull(buyWeightMachine.getDeductWeight()));
            buyWeightMachine.setNetWeight(netWeight);

            //司磅员
            buyWeightMachine.setWeightMan(userInfo.getName());

            //修改者信息
            buyWeightMachine.setModifyId(userInfo.getId());
            buyWeightMachine.setModifier(userInfo.getName());
            buyWeightMachine.setModifyTime(new Date());

            buyWeightMachineMapper.updateById(buyWeightMachine);
            return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_SUCCESS);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_FAIL);
    }


    /**
     * @Description 通知取样
     * @author chenxm66777123
     * @Date 2019/9/30 10:15
     * @version 1.0.0
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<String> samplePush(String machineId, AuthPlatformUserInfo userInfo) {
        //查询地磅信息
        BuyWeightMachine buyWeightMachine = buyWeightMachineMapper.selectOne(
                new BuyWeightMachine().setMachineId(machineId).setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(buyWeightMachine)) {
            log.error("根据地磅id查询不到地磅数据！");
            throw new BusinessException(ResCodeEnum.ERROR_NOT_FOUND, ExceptionMessageEnum.SYSTEM_NOT_FOUND);
        }

        //判断进厂重量
        if (ObjectUtils.isEmpty(buyWeightMachine.getInFactoryWeight())
                || buyWeightMachine.getInFactoryWeight().compareTo(BigDecimal.ZERO) <= 0) {
            return ResponseResult.buildResponseResult(ResCodeEnum.IN_WEIGHT_ZERO);
        }
        //true 有化验项 false 没有化验项
        boolean isHaveProductAttribut =
                configProductService.judgeHaveProductAttribute(buyWeightMachine.getProductId(), userInfo);
        //该产品无化验配置，无需通知取样
        if (!isHaveProductAttribut) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_HAVE_PRODUCT_ATTR);
        }
        // 修改推送信息
        buyWeightMachine.setSamplePushStatus(Status.TRUE.getKey());
        buyWeightMachine.setSamplePushTime(new Date());
        // 修改修改人信息
        buyWeightMachine.setModifyId(userInfo.getId());
        buyWeightMachine.setModifier(userInfo.getName());
        buyWeightMachine.setModifyTime(new Date());
        buyWeightMachineMapper.updateById(buyWeightMachine);
        return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_SUCCESS);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<String> confirmMachine(ConfirmMachineRQ confirmMachineRQ, AuthPlatformUserInfo userInfo) {
        //查询地磅信息
        BuyWeightMachine buyWeightMachine = buyWeightMachineMapper.selectOne(
                new BuyWeightMachine().setMachineId(confirmMachineRQ.getMachineId()).setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(buyWeightMachine)) {
            log.error("根据地磅id查询不到地磅数据！");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
        }
        //处理附件信息
        List<BuyWeightMachineFile> buyWeightMachineFileList = new ArrayList<>();
        if (!CollectionUtils.isEmpty(confirmMachineRQ.getFiles())) {
            buyWeightMachineFileList = BeanUtils.assemble(BuyWeightMachineFile.class, confirmMachineRQ.getFiles());
            buyWeightMachineFileList.stream().forEach(obj -> {
                obj.setMachineId(confirmMachineRQ.getMachineId());
                obj.setStatus(Status.TRUE.getKey());
                obj.setCreateId(userInfo.getId());
                obj.setCreateTime(new Date());
                obj.setCreator(userInfo.getName());
            });
            //保存磅单附件信息
            buyWeightMachineFileMapper.batchSaveFile(buyWeightMachineFileList);
        }
        //判断进厂重量
        if (ObjectUtils.isEmpty(buyWeightMachine.getInFactoryWeight())
                || buyWeightMachine.getInFactoryWeight().compareTo(BigDecimal.ZERO) <= 0) {
            return ResponseResult.buildResponseResult(ResCodeEnum.IN_WEIGHT_ZERO);
        }
        //判断出厂重量
        if (ObjectUtils.isEmpty(buyWeightMachine.getOutFactoryWeight())
                || buyWeightMachine.getOutFactoryWeight().compareTo(BigDecimal.ZERO) <= 0) {
            return ResponseResult.buildResponseResult(ResCodeEnum.OUT_WEIGHT_ZERO);
        }

        if (!ObjectUtils.isEmpty(confirmMachineRQ.getRemark())) {
            buyWeightMachine.setRemark(confirmMachineRQ.getRemark());
        }

        if (!ObjectUtils.isEmpty(confirmMachineRQ.getDeductWeight())) {
            //修改净重
            BigDecimal deduct = bigDecimalIsNull(confirmMachineRQ.getDeductWeight());
            //2019-11-04 净重字段的公式修改为：出厂重量-进厂重量-扣重
            BigDecimal netWeight = buyWeightMachine.getInFactoryWeight().subtract(buyWeightMachine.getOutFactoryWeight()).subtract(deduct);

            buyWeightMachine.setNetWeight(netWeight);
            buyWeightMachine.setDeductWeight(deduct);
        }

        //2019-12-13 如果净重等于0 或者为负 则不允许确认磅单
        if(buyWeightMachine.getNetWeight().compareTo(BigDecimal.ZERO) <= 0){
            return ResponseResult.buildResponseResult(ResCodeEnum.NET_WEIGHT_ZERO);
        }


        //是否称重，确认后置位已称重
        buyWeightMachine.setIsWeight(EnumWeightMachine.IsWeight.YES.getValue());

        //true 有化验项 false 没有化验项
        boolean isHaveProductAttribut =
                configProductService.judgeHaveProductAttribute(buyWeightMachine.getProductId(), userInfo);
        //该产品有化验配置
        if (isHaveProductAttribut) {
            //通知取样状态
            buyWeightMachine.setSamplePushStatus(Status.TRUE.getKey());
            //通知取样时间
            buyWeightMachine.setSamplePushTime(new Date());
        }
        //该产品无化验配置
        else {
            buyWeightMachine.setSamplePushStatus(Status.FALSE.getKey());
            buyWeightMachine.setQualityExamStatus(EnumSampleRelation.QualityExamStatus.NO.getKey());
            //默认产品规格
            List<ConfigProductSpecDTO> configProductSpecDTOS =
                    configProductSpecService.getProductSpecByProductId(buyWeightMachine.getProductId(), userInfo);
            if (CollectionUtils.isEmpty(configProductSpecDTOS)) {
                return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_PRODUCT_SPEC);
            }
            Integer productSpecId = configProductSpecDTOS.get(0).getId();
            String productSpecName = configProductSpecDTOS.get(0).getSpecName();
            buyWeightMachine.setProductSpecId(productSpecId);
            buyWeightMachine.setProductSpecName(productSpecName);

        }


        //修改者信息
        buyWeightMachine.setModifyId(userInfo.getId());
        buyWeightMachine.setModifier(userInfo.getName());
        buyWeightMachine.setModifyTime(new Date());
        buyWeightMachineMapper.updateAllColumnById(buyWeightMachine);

        //更新合同的到货量
        if (!StringUtils.isEmpty(buyWeightMachine.getContractBusinessId()) && !ObjectUtils.isEmpty(buyWeightMachine.getNetWeight())) {
            buyTransportSectionService.updateContractArrivalVolume(buyWeightMachine.getContractBusinessId(),
                    buyWeightMachine.getNetWeight(), userInfo);
        }

        return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_SUCCESS);

    }

    /**
     * @Description 继续运载
     * @author chenxm66777123
     * @Date 2019/10/8 9:25
     * @version 1.0.0
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<String> continueMachine(ConfirmMachineRQ confirmMachineRQ, AuthPlatformUserInfo userInfo) {
        //查询地磅信息
        BuyWeightMachine buyWeightMachine = buyWeightMachineMapper.selectOne(
                new BuyWeightMachine().setMachineId(confirmMachineRQ.getMachineId()).setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(buyWeightMachine)) {
            log.error("根据地磅id查询不到地磅数据！");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
        }

        //生成id
        String generateBusinessId = generateIdService.generateBusinessId(EnumGenerateIdModule.Module.BUY_WEIGHT_MACHINE.getKey());

        BuyWeightMachine newBuyWeightMachine = BeanUtils.copyProperties(buyWeightMachine, BuyWeightMachine.class);
        newBuyWeightMachine.setId(null);
        newBuyWeightMachine.setMachineId(generateBusinessId);

        //进行更新操作
        //处理附件信息
        List<BuyWeightMachineFile> buyWeightMachineFileList = new ArrayList<>();
        if (!CollectionUtils.isEmpty(confirmMachineRQ.getFiles())) {
            buyWeightMachineFileList = BeanUtils.assemble(BuyWeightMachineFile.class, confirmMachineRQ.getFiles());
            buyWeightMachineFileList.stream().forEach(obj -> {
                obj.setMachineId(buyWeightMachine.getMachineId());
                obj.setCreateTime(new Date());
                obj.setStatus(Status.TRUE.getKey());
                obj.setCreateId(userInfo.getId());
                obj.setCreator(userInfo.getName());
            });
            //保存磅单附件信息
            buyWeightMachineFileMapper.batchSaveFile(buyWeightMachineFileList);
        }
        //判断进厂重量
        if (ObjectUtils.isEmpty(buyWeightMachine.getInFactoryWeight())
                || buyWeightMachine.getInFactoryWeight().compareTo(BigDecimal.ZERO) <= 0) {
            return ResponseResult.buildResponseResult(ResCodeEnum.IN_WEIGHT_ZERO);
        }
        //判断出厂重量
        if (ObjectUtils.isEmpty(buyWeightMachine.getOutFactoryWeight())
                || buyWeightMachine.getOutFactoryWeight().compareTo(BigDecimal.ZERO) <= 0) {
            return ResponseResult.buildResponseResult(ResCodeEnum.OUT_WEIGHT_ZERO);
        }

        if (!ObjectUtils.isEmpty(confirmMachineRQ.getRemark())) {
            buyWeightMachine.setRemark(confirmMachineRQ.getRemark());
        }

        if (!ObjectUtils.isEmpty(confirmMachineRQ.getDeductWeight())) {
            //修改净重
            BigDecimal deduct = bigDecimalIsNull(confirmMachineRQ.getDeductWeight());
            //2019-11-04 净重字段的公式修改为：出厂重量-进厂重量-扣重
            BigDecimal netWeight = buyWeightMachine.getInFactoryWeight().subtract(buyWeightMachine.getOutFactoryWeight()).subtract(deduct);

            buyWeightMachine.setDeductWeight(deduct);
            buyWeightMachine.setNetWeight(netWeight);
        }

        //2019-12-13 如果净重等于0 或者为负 则不允许确认磅单
        if(buyWeightMachine.getNetWeight().compareTo(BigDecimal.ZERO) <= 0){
            return ResponseResult.buildResponseResult(ResCodeEnum.NET_WEIGHT_ZERO);
        }

        //是否称重，确认后置位已称重
        buyWeightMachine.setIsWeight(EnumWeightMachine.IsWeight.YES.getValue());

        //true 有化验项 false 没有化验项
        boolean isHaveProductAttribut =
                configProductService.judgeHaveProductAttribute(buyWeightMachine.getProductId(), userInfo);
        //该产品有化验配置
        if (isHaveProductAttribut) {
            //通知取样时间
            buyWeightMachine.setSamplePushTime(new Date());
            //通知取样状态
            buyWeightMachine.setSamplePushStatus(Status.TRUE.getKey());
        }
        //该产品无化验配置
        else {
            buyWeightMachine.setSamplePushStatus(Status.FALSE.getKey());
            buyWeightMachine.setQualityExamStatus(EnumSampleRelation.QualityExamStatus.NO.getKey());
            //默认产品规格
            List<ConfigProductSpecDTO> configProductSpecDTOS =
                    configProductSpecService.getProductSpecByProductId(buyWeightMachine.getProductId(), userInfo);
            if (CollectionUtils.isEmpty(configProductSpecDTOS)) {
                return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_PRODUCT_SPEC);
            }
            String productSpecName = configProductSpecDTOS.get(0).getSpecName();
            Integer productSpecId = configProductSpecDTOS.get(0).getId();
            buyWeightMachine.setProductSpecId(productSpecId);
            buyWeightMachine.setProductSpecName(productSpecName);
        }

        //修改者信息
        buyWeightMachine.setModifyId(userInfo.getId());
        buyWeightMachine.setModifier(userInfo.getName());
        buyWeightMachine.setModifyTime(new Date());
        buyWeightMachineMapper.updateById(buyWeightMachine);

        //更新合同的到货量
        if (!StringUtils.isEmpty(buyWeightMachine.getContractBusinessId()) && !ObjectUtils.isEmpty(buyWeightMachine.getNetWeight())) {
            buyTransportSectionService.updateContractArrivalVolume(buyWeightMachine.getContractBusinessId(),
                    buyWeightMachine.getNetWeight(), userInfo);
        }
        newBuyWeightMachine.setRemark("");
        //是否称重，确认后置位已称重
        newBuyWeightMachine.setIsWeight(EnumWeightMachine.IsWeight.NO.getValue());
        //通知取样状态
        newBuyWeightMachine.setSamplePushStatus(Status.FALSE.getKey());
        //通知取样时间
        newBuyWeightMachine.setSamplePushTime(new Date());
        newBuyWeightMachine.setInFactoryWeight(null);
        newBuyWeightMachine.setOutFactoryWeight(null);
        newBuyWeightMachine.setNetWeight(null);
        newBuyWeightMachine.setDeductWeight(null);
        newBuyWeightMachine.setInFactoryWeightIsConfirm(Status.FALSE.getKey());
        newBuyWeightMachine.setOutFactoryWeightIsConfirm(Status.FALSE.getKey());

        //修改者信息
        newBuyWeightMachine.setModifyId(userInfo.getId());
        newBuyWeightMachine.setModifier(userInfo.getName());
        newBuyWeightMachine.setModifyTime(new Date());
        buyWeightMachineMapper.insert(newBuyWeightMachine);

        return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_SUCCESS);
    }

    /**
     * @Description 删除磅单
     * @author chenxm66777123
     * @Date 2019/10/21 17:19
     * @version 1.0.0
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<String> delMachine(ConfirmMachineRQ confirmMachineRQ, AuthPlatformUserInfo userInfo) {
        //查询地磅信息
        BuyWeightMachine buyWeightMachine = buyWeightMachineMapper.selectOne(
                new BuyWeightMachine().setMachineId(confirmMachineRQ.getMachineId()).setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(buyWeightMachine)) {
            log.error("根据地磅id查询不到地磅数据！");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
        }
        buyWeightMachine.setStatus(Status.FALSE.getKey());
        //修改者信息
        buyWeightMachine.setModifyId(userInfo.getId());
        buyWeightMachine.setModifier(userInfo.getName());
        buyWeightMachine.setModifyTime(new Date());

        buyWeightMachineMapper.updateById(buyWeightMachine);
        return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_SUCCESS);
    }

    /**
     * @Description 六个小时执行一次删除数据任务
     * @author chenxm66777123
     * @Date 2019/10/21 9:22
     * @version 1.0.0
     */
    @Override
    public void clearWeightData() {
        buyWeightMachineFileMapper.clearWeightData();
    }


    /**
     * @Description 开始称重
     * @author chenxm66777123
     * @Date 2019/9/24 15:42
     * @version 1.0.0
     */
    @Override
    public String getWeight(String deviceId) {
        WeightMachineData buyWeightMachineData = weightMachineDataMapper.getReverseOneResult(deviceId);
        log.info("buyWeightMachineData数据：{}", buyWeightMachineData);
        if (ObjectUtils.isEmpty(buyWeightMachineData)) {
            return "0";
        }
        return new BigDecimal(buyWeightMachineData.getWeightData())
                .divide(new BigDecimal(1000).setScale(3, BigDecimal.ROUND_HALF_UP)).stripTrailingZeros().toPlainString();
    }

    /**
     * @Description 吨转化为kg
     * @author chenxm66777123
     * @Date 2019/10/13 17:31
     * @version 1.0.0
     */
    private BigDecimal tonTokg(BigDecimal data) {
        if (ObjectUtils.isEmpty(data)) {
            data = BigDecimal.ZERO;
        }
        return data.setScale(3, BigDecimal.ROUND_HALF_UP);
    }

    @Override
    public ResponseResult<SampleMachineBuyDTO> getMachineByTrainNum(String trainNumber, AuthPlatformUserInfo userInfo) {
        Page<BuyWeightMachine> pagination = new Page<>();
        pagination.setSize(1);
        Page<BuyWeightMachine> page = this.selectPage(pagination, new EntityWrapper<>(new BuyWeightMachine()
                .setEnterpriseId(userInfo.getOrgId())
                .setFactoryId(userInfo.getFactoryId())
                .setTrainNumber(trainNumber)
                .setStatus(Status.TRUE.getKey()))
                .orderBy("create_time", false));
        List<BuyWeightMachine> records = page.getRecords();
        if (CollectionUtils.isEmpty(records)) {
            log.info("为查询到对应磅单，车牌号：{}", trainNumber);
            return ResponseResult.buildResponseResult(ResCodeEnum.RECORD_NOT_FOUND);
        }
        BuyWeightMachine machine = records.get(0);
        SampleMachineBuyDTO dto = BeanUtils.copyProperties(machine, SampleMachineBuyDTO.class);
        if (machine.getWeighingTime() != null) {
            try {
                DateFormat df = new SimpleDateFormat("yyyy年MM月dd日 HH:mm:ss");
                String formatTime = df.format(machine.getWeighingTime());
                dto.setWeightDate(formatTime.substring(0, 11));
                dto.setWeightTime(formatTime.substring(12).trim());
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }

    @Override
    public ResponseResult<List<SampleMachineBuyDTO>> getSampleMachineBox(Integer productId, AuthPlatformUserInfo userInfo) {
        List<BuyWeightMachine> records = this.selectList(new EntityWrapper<>(new BuyWeightMachine()
                .setEnterpriseId(userInfo.getOrgId())
                .setFactoryId(userInfo.getFactoryId())
                .setProductId(productId)
                .setStatus(Status.TRUE.getKey()))
                .where("machine_id NOT IN ( SELECT DISTINCT b.machine_id FROM buy_sample_weight_relation b)")
                .orderBy("create_time", false));
        if (CollectionUtils.isEmpty(records)) {
            log.info("多车一样未查询到未取样磅单，类：{}，方法：{}", "BuyWeightMachineServiceImpl", "getSampleMachineBox");
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new ArrayList<>(0));
        }
        List<SampleMachineBuyDTO> dtoList = Lists.newArrayList();
        DateFormat df = new SimpleDateFormat("yyyy年MM月dd日 HH:mm:ss");
        String formatTime;
        for (BuyWeightMachine record : records) {
            try {
                SampleMachineBuyDTO dto = BeanUtils.copyProperties(record, SampleMachineBuyDTO.class);
                formatTime = df.format(record.getWeighingTime());
                dto.setWeightDate(formatTime.substring(0, 11));
                dto.setWeightTime(formatTime.substring(12).trim());
                dtoList.add(dto);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dtoList);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<String> confirmDeductWeight(String machineId, BigDecimal deductWeight,
                                                      Integer deductWeightByManual, AuthPlatformUserInfo userInfo) {
        //查询地磅信息
        BuyWeightMachine buyWeightMachine = buyWeightMachineMapper.selectOne(
                new BuyWeightMachine().setMachineId(machineId).setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(buyWeightMachine)) {
            log.error("根据地磅id查询不到地磅数据！");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
        }
        //判断进厂重量
        if (ObjectUtils.isEmpty(buyWeightMachine.getInFactoryWeight())
                || buyWeightMachine.getInFactoryWeight().compareTo(BigDecimal.ZERO) <= 0) {
            return ResponseResult.buildResponseResult(ResCodeEnum.IN_WEIGHT_ZERO);
        }
        //出厂重量
        BigDecimal outFactoryWeight = bigDecimalIsNull(buyWeightMachine.getOutFactoryWeight());
        //修改净重
        BigDecimal deduct = bigDecimalIsNull(deductWeight);
        //2019-11-04 净重字段的公式修改为：出厂重量-进厂重量-扣重
        BigDecimal netWeight = buyWeightMachine.getInFactoryWeight().subtract(outFactoryWeight).subtract(deduct);


        buyWeightMachine.setDeductWeight(deduct);
        buyWeightMachine.setDeductWeightByManual(deductWeightByManual);
        buyWeightMachine.setNetWeight(netWeight);

        //修改者信息
        buyWeightMachine.setModifyId(userInfo.getId());
        buyWeightMachine.setModifier(userInfo.getName());
        buyWeightMachine.setModifyTime(new Date());

        buyWeightMachineMapper.updateById(buyWeightMachine);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }


    /**
     * @Description 根据车牌号，自动带出最近一次该车所对应的承运商。
     * @author chenxm66777123
     * @Date 2019/11/11 9:51
     * @version 1.0.0
     */
    @Override
    public ResponseResult<CarrierInfoDTO> getLastCarrierByTrainNumber(String trainNumber, AuthPlatformUserInfo userInfo) {
        BuyWeightMachine buyWeightMachine = this.selectOne(
                new EntityWrapper<BuyWeightMachine>()
                        .where("1 =1 and status = 1 and is_weight = 1 and enterprise_id = {0} and factory_id ={1} and train_number ={2}"
                                , userInfo.getOrgId(), userInfo.getFactoryId(), trainNumber)
                        .orderBy("create_time", false).last("limit 1"));
        if (ObjectUtils.isEmpty(buyWeightMachine)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_DATA);
        }
        CarrierInfoDTO carrierInfoDTO = new CarrierInfoDTO();
        if (ObjectUtils.isEmpty(buyWeightMachine.getCarrierId())) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_DATA);
        }
        carrierInfoDTO.setCarrierId(buyWeightMachine.getCarrierId().longValue());
        carrierInfoDTO.setCarrierName(buyWeightMachine.getCarrierName());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, carrierInfoDTO);
    }


    /**
     * @Description 保存备注
     * @author chenxm66777123
     * @Date 2019/11/15 9:56
     * @version 1.0.0
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<String> saveRemark(String machineId, String remark, AuthPlatformUserInfo userInfo) {
        //查询地磅信息
        BuyWeightMachine buyWeightMachine = buyWeightMachineMapper.selectOne(
                new BuyWeightMachine().setMachineId(machineId).setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(buyWeightMachine)) {
            log.error("根据地磅id查询不到地磅数据！");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
        }
        buyWeightMachine.setRemark(remark);

        //修改者信息
        buyWeightMachine.setModifyId(userInfo.getId());
        buyWeightMachine.setModifier(userInfo.getName());
        buyWeightMachine.setModifyTime(new Date());

        buyWeightMachineMapper.updateById(buyWeightMachine);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @Override
    public ResponseResult<String> updateTrainNumber(String machineId, String trainNumber, AuthPlatformUserInfo userInfo) {
        //查询地磅信息
        BuyWeightMachine buyWeightMachine = buyWeightMachineMapper.selectOne(
                new BuyWeightMachine().setMachineId(machineId).setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(buyWeightMachine)) {
            log.error("根据地磅id查询不到地磅数据！");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
        }
        if (!EnumWeightMachine.DataSource.ADD.getValue().equals(buyWeightMachine.getDataSource())) {
            log.error("根据地磅id查询不到地磅数据！");
            return ResponseResult.buildResponseResult(ResCodeEnum.DATASOURCE_ERROR);
        }
        buyWeightMachine.setTrainNumber(trainNumber);
        //修改者信息
        buyWeightMachine.setModifyId(userInfo.getId());
        buyWeightMachine.setModifier(userInfo.getName());
        buyWeightMachine.setModifyTime(new Date());

        buyWeightMachineMapper.updateById(buyWeightMachine);

        //修改车牌号
        List<BuyGoodsPendingStorage> result = buyGoodsPendingStorageMapper.selectList(new EntityWrapper<>(
                new BuyGoodsPendingStorage()
                        .setMachineId(buyWeightMachine.getMachineId())
                        .setStatus(Status.TRUE.getKey())));
        if(!ObjectUtils.isEmpty(result)){
            result.stream().forEach(obj->{
                obj.setLicensePlateNumber(trainNumber);
                buyGoodsPendingStorageMapper.updateById(obj);
            });
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @Description web端地磅头部车辆、净重统计
     * @author chenxm66777123
     * @Date 2019/11/25 10:42
     * @version 1.0.0
     */
    @Override
    public ResponseResult<WeightMachineWebCountDTO> getWeightMachineWebCount(Integer isWeight, AuthPlatformUserInfo userInfo) {
        List<BuyWeightMachine> buyWeightMachines =  buyWeightMachineMapper.selectList(new EntityWrapper<>(new BuyWeightMachine()
                .setIsWeight(isWeight).setEnterpriseId(userInfo.getOrgId()).setStatus(Status.TRUE.getKey())));
        WeightMachineWebCountDTO weightMachineWebCountDTO = new WeightMachineWebCountDTO(0,0,BigDecimal.ZERO);
        if(ObjectUtils.isEmpty(buyWeightMachines)){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,weightMachineWebCountDTO);
        }
        Integer size = buyWeightMachines.size();
        if(EnumWeightMachine.IsWeight.NO.getValue().equals(isWeight)){
            weightMachineWebCountDTO.setWaitWeightCar(size);
        }
        if(EnumWeightMachine.IsWeight.YES.getValue().equals(isWeight)){
            weightMachineWebCountDTO.setAlreadyWeightCar(size);
            BigDecimal weight = BigDecimal.ZERO;
            for (BuyWeightMachine buyWeightMachine : buyWeightMachines) {
                weight = weight.add(buyWeightMachine.getNetWeight());
            }
            weightMachineWebCountDTO.setAlreadyNetWeight(weight);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,weightMachineWebCountDTO);
    }

    /**
     * @Description web获取待过磅/已过磅列表信息
     * @author chenxm66777123
     * @Date 2019/11/25 14:12
     * @version 1.0.0
     */
    @Override
    public List<WeightMachineWebDTO> getWeightMachineWebList(WeightMachineWebListRq rq,
                  AuthPlatformUserInfo userInfo, Pagination pagination) {

        EntityWrapper wrapper = new EntityWrapper();
        wrapper.eq("is_weight", rq.getIsWeight());
        wrapper.eq("enterprise_id", userInfo.getOrgId());
        wrapper.eq("status", Status.TRUE.getKey());
        //车牌号
        if(!ObjectUtils.isEmpty(rq.getTrainNumber())){
            wrapper.like("train_number", rq.getTrainNumber());
        }
        //合同号
        if(!ObjectUtils.isEmpty(rq.getContractNum())){
            wrapper.like("contract_num", rq.getContractNum());
        }

        //带称重时间
        if(EnumWeightMachine.IsWeight.NO.getValue().equals(rq.getIsWeight())){

            if (!ObjectUtils.isEmpty(rq.getStartTime()) && !ObjectUtils.isEmpty(rq.getEndTime())) {
                wrapper.between("create_time",rq.getStartTime() + DateUtils.TIME_SUFFIX ,rq.getEndTime() +  DateUtils.TIME_END );
            }
            if (!ObjectUtils.isEmpty(rq.getStartTime()) && ObjectUtils.isEmpty(rq.getEndTime())) {
                wrapper.ge("create_time",rq.getStartTime() + DateUtils.TIME_SUFFIX);

            }
            if (ObjectUtils.isEmpty(rq.getStartTime()) && !ObjectUtils.isEmpty(rq.getEndTime())) {
                wrapper.le("create_time",rq.getEndTime() +  DateUtils.TIME_END );
            }
            wrapper.orderBy("create_time",false);
        }
        //已称重
        if(EnumWeightMachine.IsWeight.YES.getValue().equals(rq.getIsWeight())){
            if (!ObjectUtils.isEmpty(rq.getStartTime()) && !ObjectUtils.isEmpty(rq.getEndTime())) {
                wrapper.between("weighing_time",rq.getStartTime() + DateUtils.TIME_SUFFIX ,rq.getEndTime() +  DateUtils.TIME_END );
            }
            if (!ObjectUtils.isEmpty(rq.getStartTime()) && ObjectUtils.isEmpty(rq.getEndTime())) {
                wrapper.ge("weighing_time",rq.getStartTime() + DateUtils.TIME_SUFFIX);

            }
            if (ObjectUtils.isEmpty(rq.getStartTime()) && !ObjectUtils.isEmpty(rq.getEndTime())) {
                wrapper.le("weighing_time",rq.getEndTime() +  DateUtils.TIME_END );
            }
            wrapper.orderBy("weighing_time",false);
        }
        //发货单位
        if(!ObjectUtils.isEmpty(rq.getSupOrCustId())){
            wrapper.eq("delivery_company_id", rq.getSupOrCustId());
        }
        List<WeightMachineWebDTO> result = new ArrayList<>();
        List<BuyWeightMachine> buyWeightMachines = buyWeightMachineMapper.selectPage(pagination, wrapper);
        if(CollectionUtils.isEmpty(buyWeightMachines)){
            return result;
        }
        result = BeanUtils.assemble(WeightMachineWebDTO.class, buyWeightMachines);
        return result;
    }

    /**
     * @Description web根据磅单号获取详细信息
     * @author chenxm66777123
     * @Date 2019/11/25 16:04
     * @version 1.0.0
     */
    @Override
    public ResponseResult<WeightMachineWebDTO> getWeightMachineWebDeatil(String machineId) {
        //查询地磅信息
        BuyWeightMachine buyWeightMachine = buyWeightMachineMapper.selectOne(
                new BuyWeightMachine().setMachineId(machineId).setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(buyWeightMachine)) {
            log.error("根据地磅id查询不到地磅数据！");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
        }
        WeightMachineWebDTO weightMachineWebDTO = BeanUtils.copyProperties(buyWeightMachine, WeightMachineWebDTO.class);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,weightMachineWebDTO);
    }


    /**
     * @Description 磅单绑定合同号列表查询
     * @author chenxm66777123
     * @Date 2019/11/26 10:28
     * @version 1.0.0
     */
    @Override
    public List<WeightMachineWebBindDTO> getWeightMachineWebBindList(WeightMachineBindRq rq, AuthPlatformUserInfo userInfo, Pagination pagination) {
        Map<String,Object> map = new HashMap<>();
        map.put("enterpriseId", userInfo.getOrgId());
        map.put("isIgnore", rq.getIsIgnore());
        if (!ObjectUtils.isEmpty(rq.getCustOrSupName())) {
            map.put("custOrSupName",rq.getCustOrSupName());
        }
        //过磅日期
        if (!ObjectUtils.isEmpty(rq.getStartTime()) && !ObjectUtils.isEmpty(rq.getEndTime())) {
            map.put("startTime",rq.getStartTime() + DateUtils.TIME_SUFFIX);
            map.put("endTime",rq.getEndTime() +  DateUtils.TIME_END);
        }
        if (!ObjectUtils.isEmpty(rq.getStartTime()) && ObjectUtils.isEmpty(rq.getEndTime())) {
            map.put("startTime",rq.getStartTime() + DateUtils.TIME_SUFFIX);
        }
        if (ObjectUtils.isEmpty(rq.getStartTime()) && !ObjectUtils.isEmpty(rq.getEndTime())) {
            map.put("endTime",rq.getEndTime() +  DateUtils.TIME_END);
        }

        List<WeightMachineWebBindDTO>  weightMachineWebBindList = buyWeightMachineMapper.getWeightMachineWebBindList(map,pagination);
        return weightMachineWebBindList;
    }

    /**
     * @Description web磅单获取管理的合同号
     * @author chenxm66777123
     * @Date 2019/11/26 15:14
     * @version 1.0.0
     */
    @Override
    public List<WeightMachineWebRelationContractDTO> getRelationContract(AuthPlatformUserInfo userInfo,String custOrSupName,String productName, Pagination pagination) {
        Map<String,Object> map = new HashMap<>();
        map.put("enterpriseId", userInfo.getOrgId());
        map.put("custOrSupName", custOrSupName);
        map.put("productName", productName);
        List<WeightMachineWebRelationContractDTO> result  =  buyContractBasicMapper.getRelationContract(map,pagination);
        return result;
    }

    /**
     * @Description web磅单忽略/恢复
     * @author chenxm66777123
     * @Date 2019/11/26 15:55
     * @version 1.0.0
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<String> ignoreRecoveryMachine(WeightIgnoreMachineRq rq) {
        //查询地磅信息
        BuyWeightMachine buyWeightMachine = buyWeightMachineMapper.selectOne(
                new BuyWeightMachine().setMachineId(rq.getMachineId()).setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(buyWeightMachine)) {
            log.error("根据地磅id查询不到地磅数据！");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
        }
        buyWeightMachine.setBindIgnore(rq.getIsIgnore());
        buyWeightMachineMapper.updateById(buyWeightMachine);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @Description web磅单绑定合同号
     * @author chenxm66777123
     * @Date 2019/11/26 17:10
     * @version 1.0.0
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void batchBindContract(List<Map<String, Object>> param) {
        buyWeightMachineMapper.batchBindContract(param);

        //修改合同号
        for (Map<String, Object> map : param) {
           String machineId =  map.get("machineId").toString();
           String contractNum = map.get("contractNum").toString();
            //修改合同号
            List<BuyGoodsPendingStorage> result = buyGoodsPendingStorageMapper.selectList(new EntityWrapper<>(
                    new BuyGoodsPendingStorage()
                            .setMachineId(machineId)
                            .setStatus(Status.TRUE.getKey())));
            if(!ObjectUtils.isEmpty(result)){
                result.stream().forEach(obj->{
                    obj.setContractId(contractNum);
                    buyGoodsPendingStorageMapper.updateById(obj);
                });
            }
        }
    }

    private BigDecimal bigDecimalIsNull(BigDecimal data) {
        if (ObjectUtils.isEmpty(data)) {
            return BigDecimal.ZERO;
        }
        if (data.compareTo(BigDecimal.ZERO) == 0) {
            return BigDecimal.ZERO;
        }
        return data.setScale(3, BigDecimal.ROUND_HALF_UP);
    }
}
