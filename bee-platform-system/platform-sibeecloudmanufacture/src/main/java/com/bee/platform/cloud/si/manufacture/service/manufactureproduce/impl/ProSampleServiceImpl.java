package com.bee.platform.cloud.si.manufacture.service.manufactureproduce.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.dao.mapper.ProSampleMapper;
import com.bee.platform.cloud.si.manufacture.dto.SampleAssayDetailDTO;
import com.bee.platform.cloud.si.manufacture.dto.SampleAssayResultDTO;
import com.bee.platform.cloud.si.manufacture.entity.BarCode;
import com.bee.platform.cloud.si.manufacture.entity.ProOreFurnaceSample;
import com.bee.platform.cloud.si.manufacture.entity.ProSample;
import com.bee.platform.cloud.si.manufacture.entity.SampleAssayResult;
import com.bee.platform.cloud.si.manufacture.enums.EnumSampleRelation;
import com.bee.platform.cloud.si.manufacture.rq.*;
import com.bee.platform.cloud.si.manufacture.service.BarCodeService;
import com.bee.platform.cloud.si.manufacture.service.SampleService;
import com.bee.platform.cloud.si.manufacture.service.manufacturebuy.SampleAssayResultService;
import com.bee.platform.cloud.si.manufacture.service.manufactureproduce.ProOreFurnaceRecordService;
import com.bee.platform.cloud.si.manufacture.service.manufactureproduce.ProOreFurnaceSampleService;
import com.bee.platform.cloud.si.manufacture.service.manufactureproduce.ProSampleService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.utils.BeanUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import javax.validation.constraints.NotBlank;
import java.util.Date;
import java.util.List;
import java.util.Objects;

/**
 * <p>
 * 生产样品表 服务实现类
 * </p>
 *
 * @author liliang123
 * @since 2019-09-27
 */
@Slf4j
@Service
public class ProSampleServiceImpl extends ServiceImpl<ProSampleMapper, ProSample> implements ProSampleService {

    @Autowired
    private ProOreFurnaceSampleService proOreFurnaceSampleService;
    @Autowired
    private SampleService sampleService;
    @Autowired
    private BarCodeService barCodeService;
    @Autowired
    private ProOreFurnaceRecordService proOreFurnaceRecordService;
    @Autowired
    private SampleAssayResultService sampleAssayResultService;

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> saveSample(SaveSampleProRQ rq, AuthPlatformUserInfo userInfo) {
        Integer furnaceId = rq.getFurnaceId();
        Integer shift = rq.getShift();
        Integer furnaceBatch = rq.getFurnaceBatch();
        BarCode barCode = barCodeService.checkCodeExist(rq.getSampleCode());
        if (barCode == null) {
            log.info("未找到对应的条形码，码：{}", rq.getSampleCode());
            return ResponseResult.buildResponseResult(ResCodeEnum.CODE_NOT_EXIST);
        } else if (Objects.equals(barCode.getUsed(), Status.TRUE.getKey())) {
            log.info("条形码已使用，码：{}", rq.getSampleCode());
            return ResponseResult.buildResponseResult(ResCodeEnum.CODE_ALREADY_USED);
        }
        // 检查当前炉号班次炉次 是否已取样
        List<ProSample> sampleList = this.checkIfSample(furnaceId, shift, furnaceBatch, rq.getOpenTime(), userInfo);
        if (sampleList != null) {
            log.info("生产确定取样-已经取样且该样品未弃用，炉号：{}，班次：{}，炉次：{}，公司：{}，工厂：{}，样品:{}",
                    furnaceId, shift, furnaceBatch, userInfo.getOrgId(), userInfo.getFactoryId(), sampleList);
            return ResponseResult.buildResponseResult(ResCodeEnum.ALREADY_PRO_SAMPLE);
        }
        // 如果是新增的-需要先添加矿热炉-炉次信息记录
        ProOreFurnaceSample proOreFurnaceSample = null;
        if (Objects.equals(rq.getNewAdd(), Status.TRUE.getKey())) {
            // 查询是否已通知取样
            int count = proOreFurnaceSampleService.getNoticeSampleCount(furnaceId, shift, furnaceBatch, rq.getOpenTime());
            if (count > 0) {
                log.info("当前炉次已经通知过取样,公司：{}，工厂：{}，炉号：{}，班次：{}，批次：{}，开班时间：{}",
                        userInfo.getOrgId(), userInfo.getFactoryId(), rq.getFurnaceId(), rq.getShift(), rq.getFurnaceBatch(), rq.getOpenTime());
                return ResponseResult.buildResponseResult(ResCodeEnum.ORE_FURNACE_SAMPLE_NOTICE_EXIST);
            }
            // 手动输入 记录通知取样记录 并将取样状态设置为，未取样-将显示在待取样列表：需要手动完成取样
            proOreFurnaceSample = new ProOreFurnaceSample()
                    .setOpenTime(rq.getOpenTime())
                    .setFurnaceId(rq.getFurnaceId())
                    .setShift(rq.getShift())
                    .setFurnaceBatch(rq.getFurnaceBatch())
                    .setSampleStatus(EnumSampleRelation.SampleStatus.NOT_YET.getKey())
                    .setStatus(Status.TRUE.getKey())
                    .setCompanyId(userInfo.getOrgId())
                    .setFactoryId(userInfo.getFactoryId())
                    .setCreateId(userInfo.getId())
                    .setCreateTime(new Date());
            proOreFurnaceSampleService.insert(proOreFurnaceSample);
        } else {
            // 如果不是手动输入  则查询通知记录  并将状态设置为已取样
            proOreFurnaceSample = proOreFurnaceSampleService.selectOne(new EntityWrapper<>(new ProOreFurnaceSample()
                    .setOpenTime(rq.getOpenTime())
                    .setFurnaceId(rq.getFurnaceId())
                    .setShift(rq.getShift())
                    .setFurnaceBatch(rq.getFurnaceBatch())
                    .setCompanyId(userInfo.getOrgId())
                    .setFactoryId(userInfo.getFactoryId())));
        }
        // 新增取样数据
        ProSample sample = BeanUtils.copyProperties(rq, ProSample.class);
        if (proOreFurnaceSample != null) {
            sample.setProOreFurnaceSampleId(proOreFurnaceSample.getId());
        }
        sample.setAssayStatus(EnumSampleRelation.SampleAssayStatus.PREPARE_ASSAY.getKey())
                .setEnterpriseId(userInfo.getOrgId())
                .setFactoryId(userInfo.getFactoryId())
                .setCreateId(userInfo.getId())
                .setCreator(userInfo.getName())
                .setCreateTime(new Date());
        this.insert(sample);
        // 更新条形码编号为已使用
        barCodeService.updateCodeUsed(barCode);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> finishSample(FinishSampleProRQ rq, AuthPlatformUserInfo userInfo) {
        ProOreFurnaceSample furnaceSample = proOreFurnaceSampleService.selectOne(new EntityWrapper<>(new ProOreFurnaceSample()
                .setFurnaceId(rq.getFurnaceId())
                .setShift(rq.getShift())
                .setFurnaceBatch(rq.getBakedBatch())
                .setCreateTime(rq.getSamplePushTime())
                .setStatus(Status.TRUE.getKey())));
        if (furnaceSample == null) {
            log.error("未查询到相应的炉次通知记录 类:{},FurnaceId:{},Shift:{},FurnaceBatch:{}，推送时间：{}",
                    "ProSampleServiceImpl", "ProSampleServiceImpl", rq.getFurnaceId(), rq.getShift(), rq.getBakedBatch(), rq.getSamplePushTime());
            return ResponseResult.buildResponseResult(ResCodeEnum.RECORD_NOT_FOUND);
        }
        proOreFurnaceSampleService.updateById(
                furnaceSample.setSampleStatus(EnumSampleRelation.SampleStatus.COMPLETED.getKey())
                        .setModifyId(userInfo.getId())
                        .setModifyTime(new Date()));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> startAssaySample(SampleAssayStartRQ rq, Object o, AuthPlatformUserInfo userInfo) {
        @NotBlank String sampleCode = rq.getSampleCode();
        if (o == null) {
            log.info("未找到生产样品--样品编号SampleCode：{}", sampleCode);
            return ResponseResult.buildResponseResult(ResCodeEnum.SMAPLE_NOT_FOUND);
        }
        ProSample sample = (ProSample) o;
        sample.setAssayStatus(EnumSampleRelation.SampleAssayStatus.ASSAYING.getKey())
                .setModifier(userInfo.getName())
                .setModifyId(userInfo.getId())
                .setModifyTime(new Date());
        this.updateById(sample);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> saveAssayResult(SampleAssayResultSaveRQ rq, AuthPlatformUserInfo userInfo) {
        String sampleCode = rq.getSampleCode();
        Integer productId = rq.getProductId();
        List<SampleAssayResultDTO> resultList = rq.getResultList();
        // 查询对应样品
        ProSample sample = this.selectOne(new EntityWrapper<>(new ProSample()
                .setSampleCode(sampleCode)
                .setEnterpriseId(userInfo.getOrgId())
                .setStatus(1)));
        if (sample == null) {
            log.info("未查询到该样品，方法：{}sampleCode:{} ", "saveAssayResult", sampleCode);
            return ResponseResult.buildResponseResult(ResCodeEnum.SAMPLE_NOT_FOUND);
        }
        // 多次保存 将以前保存的化验结果逻辑删除
        sampleAssayResultService.update(new SampleAssayResult().setStatus(0),
                new EntityWrapper<>(new SampleAssayResult()
                        .setSampleCode(rq.getSampleCode())
                        .setStatus(1)));
        // 查询样品输出项,并计算保存输出项结果
        sampleService.computeAndsaveAssayResult(sampleCode, productId, resultList,
                EnumSampleRelation.SampleAssayResultBusinessType.PRODUCE.getKey(), userInfo);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> abandonSample(SampleAssayAbandonRQ rq, AuthPlatformUserInfo userInfo) {
        ProSample sample = this.selectOne(new EntityWrapper<>(new ProSample()
                .setSampleCode(rq.getSampleCode())
                .setStatus(1)));
        if (sample == null) {
            log.info("未查询到该样品，类：{}，方法：{},sampleCode:{} ", "ProSampleServiceImpl", "saveAssayResult", rq.getSampleCode());
            return ResponseResult.buildResponseResult(ResCodeEnum.SAMPLE_NOT_FOUND);
        }
        sample.setAssayStatus(EnumSampleRelation.SampleAssayStatus.ABANDON.getKey())
                .setAbandonReason(rq.getAbandonReason())
                .setAbandonPerson(userInfo.getName())
                .setAbandonId(userInfo.getId())
                .setAbandonTime(new Date())
                .setModifyId(userInfo.getId())
                .setModifier(userInfo.getName())
                .setModifyTime(new Date());
        this.updateById(sample);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @Override
    public ResponseResult<SampleAssayDetailDTO> getSampleAssayDetailByCode(Object o, String sampleCode) {
        ProSample sample = (ProSample) o;
        if (!Objects.equals(sample.getAssayStatus(), EnumSampleRelation.SampleAssayStatus.ASSAYING.getKey())) {
            log.info("改条码的样品不在生产化验中样品的列表，样品code：{}", sampleCode);
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_IN_PREPARE_ASSAY);
        }
        SampleAssayDetailDTO dto = BeanUtils.copyProperties(sample, SampleAssayDetailDTO.class);
        dto.setBusinessType(EnumSampleRelation.SampleAssayResultBusinessType.PRODUCE.getKey());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }

    /**
     * 查询是否取样 且未弃样 生产每个炉次只能取样一次
     */
    @Override
    public List<ProSample> checkIfSample(Integer furnaceId, Integer shift, Integer furnaceBatch, Date openTime, AuthPlatformUserInfo userInfo) {
        List<ProSample> sampleList = this.selectList(new EntityWrapper<>(new ProSample()
                .setFurnaceId(furnaceId)
                .setShift(shift)
                .setFurnaceBatch(furnaceBatch)
                .setOpenTime(openTime)
                .setEnterpriseId(userInfo.getOrgId())
                .setFactoryId(userInfo.getFactoryId())
                .setStatus(Status.TRUE.getKey()))
                .notIn("assay_status", EnumSampleRelation.SampleAssayStatus.ABANDON.getKey()));
        if (CollectionUtils.isEmpty(sampleList)) {
            return null;
        }
        return sampleList;
    }

}
