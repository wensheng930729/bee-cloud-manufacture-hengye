package com.bee.platform.cloud.si.manufacture.service.manufactureproduce.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.dao.mapper.ProBaggingMapper;
import com.bee.platform.cloud.si.manufacture.dao.mapper.ProSampleMapper;
import com.bee.platform.cloud.si.manufacture.dao.mapper.SampleAssayResultMapper;
import com.bee.platform.cloud.si.manufacture.dto.*;
import com.bee.platform.cloud.si.manufacture.entity.BarCode;
import com.bee.platform.cloud.si.manufacture.entity.ProBagging;
import com.bee.platform.cloud.si.manufacture.entity.ProSample;
import com.bee.platform.cloud.si.manufacture.entity.SampleAssayResult;
import com.bee.platform.cloud.si.manufacture.enums.EnumProBagging;
import com.bee.platform.cloud.si.manufacture.enums.EnumSampleRelation;
import com.bee.platform.cloud.si.manufacture.rq.ProBaggingFirstRq;
import com.bee.platform.cloud.si.manufacture.rq.ProBaggingSecondRq;
import com.bee.platform.cloud.si.manufacture.service.BarCodeService;
import com.bee.platform.cloud.si.manufacture.service.StorageService;
import com.bee.platform.cloud.si.manufacture.service.manufactureproduce.ProBaggingService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.utils.BeanUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Objects;

/**
 * @author chenxm66777123
 * @version 1.0.0
 * @Description 成品装袋 服务实现类
 * @Date 2019/10/9 9:55
 */
@Service
public class ProBaggingServiceImpl extends ServiceImpl<ProBaggingMapper, ProBagging> implements ProBaggingService {

    @Autowired
    private ProBaggingMapper proBaggingMapper;
    @Autowired
    private ProSampleMapper proSampleMapper;
    @Autowired
    private SampleAssayResultMapper sampleAssayResultMapper;
    @Autowired
    private StorageService storageService;
    @Autowired
    private BarCodeService barCodeService;

    /**
     * @Description 保存成品装袋（第一步）
     * @author chenxm66777123
     * @Date 2019/10/9 10:05
     * @version 1.0.0
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ProBaggingFirstDTO> saveBaggingStepOne(ProBaggingFirstRq rq, AuthPlatformUserInfo userInfo) {

        BarCode barCode = barCodeService.checkCodeExist(rq.getBaggingCode());
        if (barCode == null) {
            return ResponseResult.buildResponseResult(ResCodeEnum.CODE_NOT_EXIST);
        } else if (Objects.equals(barCode.getUsed(), Status.TRUE.getKey())) {
            return ResponseResult.buildResponseResult(ResCodeEnum.CODE_ALREADY_USED);
        }
        //判断吨袋是否被使用过
        ProBagging proBagging =  proBaggingMapper.selectOne(
                new ProBagging().setBaggingCode(rq.getBaggingCode())
                        .setShiftTime(rq.getShiftTime())
                        .setFactoryId(rq.getFurnaceId())
                        .setFurnaceBatchCode(rq.getFurnaceBatchCode())
                        .setShiftCode(rq.getShiftCode())
                        .setStatus(Status.FALSE.getKey())
                        .setStep(EnumProBagging.Step.ONE.getKey()));
        if(!ObjectUtils.isEmpty(proBagging)){
            ProBaggingFirstDTO proBaggingFirstDTO = BeanUtils.copyProperties(rq, ProBaggingFirstDTO.class);
            proBaggingFirstDTO.setProductId(proBagging.getProductId());
            proBaggingFirstDTO.setProductName(proBagging.getProductName());
            proBaggingFirstDTO.setProductSpecId(proBagging.getProductSpecId());
            proBaggingFirstDTO.setProductSpecName(proBagging.getProductSpecName());
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,proBaggingFirstDTO);
        }

        ProBagging delBarCodeInfo =  proBaggingMapper.selectOne(
                new ProBagging().setBaggingCode(rq.getBaggingCode())
                        .setStatus(Status.FALSE.getKey())
                        .setStep(EnumProBagging.Step.ONE.getKey()));
        if(!ObjectUtils.isEmpty(delBarCodeInfo)){
            proBaggingMapper.deleteById(delBarCodeInfo);
        }

        //未被使用过做与质检结果绑定操作
        ProSample proSample =  proSampleMapper.getResultForBindBagging(rq);
        if(ObjectUtils.isEmpty(proSample)){
            return ResponseResult.buildResponseResult(ResCodeEnum.SAMPLE_NOT_FOUND);
        }

        ProBagging saveProBagging = BeanUtils.copyProperties(rq, ProBagging.class);
        saveProBagging.setEnterpriseId(userInfo.getOrgId());
        saveProBagging.setFactoryId(userInfo.getFactoryId());
        //样品编号
        saveProBagging.setSampleCode(proSample.getSampleCode());
        //产品信息
        saveProBagging.setProductId(proSample.getProductId());
        saveProBagging.setProductName(proSample.getProductName());
        //产品规格信息
        saveProBagging.setProductSpecId(proSample.getProductSpecId());
        saveProBagging.setProductSpecName(proSample.getProductSpecName());

        saveProBagging.setStatus(Status.FALSE.getKey());

        //创建者信息
        saveProBagging.setCreateId(userInfo.getId());
        saveProBagging.setCreator(userInfo.getUsername());
        saveProBagging.setCreateTime(new Date());
        saveProBagging.setModifyId(userInfo.getId());
        saveProBagging.setModifier(userInfo.getUsername());
        saveProBagging.setModifyTime(new Date());
        saveProBagging.setStep(EnumProBagging.Step.ONE.getKey());
        proBaggingMapper.insert(saveProBagging);

        ProBaggingFirstDTO proBaggingFirstDTO = BeanUtils.copyProperties(rq, ProBaggingFirstDTO.class);
        proBaggingFirstDTO.setProductId(proSample.getProductId());
        proBaggingFirstDTO.setProductName(proSample.getProductName());
        proBaggingFirstDTO.setProductSpecId(proSample.getProductSpecId());
        proBaggingFirstDTO.setProductSpecName(proSample.getProductSpecName());

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,proBaggingFirstDTO);
    }

    /**
     * @Description 保存成品装袋（第二步）
     * @author chenxm66777123
     * @Date 2019/10/9 10:05
     * @version 1.0.0
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<String> saveBaggingStepTwo(ProBaggingSecondRq rq, AuthPlatformUserInfo userInfo) {

        BarCode barCode = barCodeService.checkCodeExist(rq.getBaggingCode());
        if (barCode == null) {
            return ResponseResult.buildResponseResult(ResCodeEnum.CODE_NOT_EXIST);
        } else if (Objects.equals(barCode.getUsed(), Status.TRUE.getKey())) {
            return ResponseResult.buildResponseResult(ResCodeEnum.CODE_ALREADY_USED);
        }

        //查询数据
        ProBagging proBagging =  proBaggingMapper.selectOne(
                new ProBagging().setBaggingCode(rq.getBaggingCode()));
        if(ObjectUtils.isEmpty(proBagging)){
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
        }

        proBagging.setDeviceId(rq.getDeviceId());
        proBagging.setAmount(rq.getAmount().divide(new BigDecimal(1000).setScale(3,BigDecimal.ROUND_HALF_UP)));

        proBagging.setStatus(Status.TRUE.getKey());
        proBagging.setStep(EnumProBagging.Step.TWO.getKey());
        proBagging.setModifyId(userInfo.getId());
        proBagging.setModifier(userInfo.getUsername());
        proBagging.setModifyTime(new Date());
        proBaggingMapper.updateById(proBagging);

        // 更新条形码编号为已使用
        barCodeService.updateCodeUsed(barCode);

        //生成待入库产成品数据
        FinishedProductPendingStorageDTO finishedProductPendingStorageDTO = new FinishedProductPendingStorageDTO();
        finishedProductPendingStorageDTO.setTonBagNumber(rq.getBaggingCode())
                .setScheduling(proBagging.getShift()).setFurnaceTimes(proBagging.getFurnaceBatch()).setFurnaceNumber(proBagging.getFurnaceName())
                .setProductId(proBagging.getProductId()).setProductName(proBagging.getProductName())
                .setProductNumber(rq.getAmount().divide(new BigDecimal(1000).setScale(3,BigDecimal.ROUND_HALF_UP))).setProductUnit("吨")
                .setProductSpecId(proBagging.getProductSpecId()).setProductSpecName(proBagging.getProductSpecName());
        ;
        storageService.saveFinishedProductStorage(finishedProductPendingStorageDTO,userInfo);


        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }


    /**
     * @Description 获取装袋记录列表
     * @author chenxm66777123
     * @Date 2019/10/9 14:17
     * @version 1.0.0
     */
    @Override
    public List<ProBaggingDTO> getBaggingResults(AuthPlatformUserInfo userInfo, Pagination pagination) {

        List<ProBaggingDTO> results = proBaggingMapper.getBaggingResults(userInfo.getOrgId(),pagination);
        return results;
    }



    @Override
    public ProBaggingDeatilsDTO getBaggingDeatils(ProBaggingDTO rq, AuthPlatformUserInfo userInfo) {
        ProBaggingDeatilsDTO proBaggingDeatilsDTO = BeanUtils.copyProperties(rq, ProBaggingDeatilsDTO.class);

        //根据班次，炉号，炉此，开班时间查询记录
        List<ProBagging> proBaggings =  proBaggingMapper.selectList(new EntityWrapper<>(
                new ProBagging().setFurnaceBatchCode(rq.getFurnaceBatchCode())
                        .setFurnaceId(rq.getFurnaceId()).setShiftCode(rq.getShiftCode())
                .setShiftTime(rq.getShiftTime()).setStatus(Status.TRUE.getKey())
        ).orderBy("create_time",false));
        //样品编号
        String sampleCode = "";
        //吨袋信息
        List<ProBaggingInfo> proBaggingInfos = new ArrayList<>();
        //不为空处理
        if(!CollectionUtils.isEmpty(proBaggings)){
            sampleCode = proBaggings.get(0).getSampleCode();

            proBaggings.stream().forEach(obj->{
                ProBaggingInfo proBaggingInfo = new ProBaggingInfo();
                proBaggingInfo.setBaggingCode(obj.getBaggingCode());
                proBaggingInfo.setAmount(obj.getAmount());
                proBaggingInfo.setRealBaggingDate(obj.getCreateTime());
                proBaggingInfos.add(proBaggingInfo);
            });
        }
        proBaggingDeatilsDTO.setProBaggingInfos(proBaggingInfos);

        List<AssayItem> assayItemList = new ArrayList<>();
        //样品编号不为空处理
        if(!StringUtils.isBlank(sampleCode)){

            List<SampleAssayResult> sampleAssayResults =  sampleAssayResultMapper.selectList(new EntityWrapper<>(
                    new SampleAssayResult().setSampleCode(sampleCode)
                    .setBusinessType(EnumSampleRelation.SampleAssayResultBusinessType.PRODUCE.getKey())
                    //输出项，化验结果
                    .setType(Status.TRUE.getKey()).setStatus(Status.TRUE.getKey())
            ));
           assayItemList = BeanUtils.assemble(AssayItem.class, sampleAssayResults);

        }
        proBaggingDeatilsDTO.setAssayItemList(assayItemList);
        proBaggingDeatilsDTO.setSampleCode(sampleCode);

        return proBaggingDeatilsDTO;
    }

    /**
     * @descriptin 统计产品回收率
     * @author xin.huang
     * @param num
     * @date 2019/10/18
     * @return
     */
    private void statisticRecoveryRate(BigDecimal num) {

    }

}
