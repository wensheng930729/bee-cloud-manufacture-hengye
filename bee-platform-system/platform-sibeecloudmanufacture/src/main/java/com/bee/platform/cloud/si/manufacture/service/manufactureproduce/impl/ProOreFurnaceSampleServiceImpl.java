package com.bee.platform.cloud.si.manufacture.service.manufactureproduce.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.dao.mapper.ProOreFurnaceSampleMapper;
import com.bee.platform.cloud.si.manufacture.entity.ProOreFurnaceSample;
import com.bee.platform.cloud.si.manufacture.entity.ProSample;
import com.bee.platform.cloud.si.manufacture.rq.ProOreFurnaceSampleRQ;
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

import java.util.Date;
import java.util.List;

/**
 * <p>
 * 服务实现类
 * </p>
 *
 * @author huangxin123
 * @since 2019-10-08
 */
@Slf4j
@Service
public class ProOreFurnaceSampleServiceImpl extends ServiceImpl<ProOreFurnaceSampleMapper, ProOreFurnaceSample> implements ProOreFurnaceSampleService {

    @Autowired
    private ProOreFurnaceRecordService oreFurnaceRecordService;
    @Autowired
    private ProSampleService proSampleService;

    /**
     * @param rq
     * @param userInfo
     * @return
     * @descriptin 通知取样
     * @author xin.huang
     * @date 2019/10/8
     */
    @Override
    public ResponseResult noticeSample(ProOreFurnaceSampleRQ rq, AuthPlatformUserInfo userInfo) {
        Integer furnaceId = rq.getFurnaceId();
        Integer shift = rq.getShift();
        Integer furnaceBatch = rq.getFurnaceBatch();
        // 检查当前炉号班次炉次 是否已取样
        List<ProSample> sampleList = proSampleService.checkIfSample(furnaceId, shift, furnaceBatch, rq.getOpenTime(), userInfo);
        if (sampleList != null) {
            log.info("生产通知取样--已经取样且该样品未弃用，炉号：{}，班次：{}，炉次：{}，公司：{}，工厂：{}，开班时间：{}，样品:{}",
                    furnaceId, shift, furnaceBatch, userInfo.getOrgId(), userInfo.getFactoryId(), rq.getOpenTime(), sampleList);
            return ResponseResult.buildResponseResult(ResCodeEnum.ALREADY_PRO_SAMPLE);
        }
        // 查询是否已通知取样
        int count = this.getNoticeSampleCount(furnaceId, shift, furnaceBatch, rq.getOpenTime());
        if (count > 0) {
            log.info("当前炉次已经通知过取样,炉号：{}，班次：{}，批次：{}，开班时间：{}",
                    rq.getFurnaceId(), rq.getShift(), rq.getFurnaceBatch(), rq.getOpenTime());
            return ResponseResult.buildResponseResult(ResCodeEnum.ORE_FURNACE_SAMPLE_NOTICE_EXIST);
        }
        // 记录通知取样信息
        ProOreFurnaceSample sample = BeanUtils.copyProperties(rq, ProOreFurnaceSample.class);
        sample.setCreateId(userInfo.getId())
                .setCompanyId(userInfo.getOrgId())
                .setFactoryId(userInfo.getFactoryId())
                .setOpenTime(rq.getOpenTime())
                .setCreateTime(new Date());
        if (!this.insert(sample)) {
            log.error("通知取样失败 类：{} 方法：{}", "ProOreFurnaceSampleServiceImpl", "noticeSample");
            return ResponseResult.buildResponseResult(ResCodeEnum.ADD_FAILED);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @Override
    public int getNoticeSampleCount(Integer furnaceId, Integer shift, Integer furnaceBatch, Date openTime) {
        // 查询是否已通知取样
        return this.selectCount(new EntityWrapper<>(new ProOreFurnaceSample()
                .setOpenTime(openTime)
                .setFurnaceId(furnaceId)
                .setShift(shift)
                .setFurnaceBatch(furnaceBatch)
                // 未取样
                .setSampleStatus(Status.FALSE.getKey())
                .setStatus(Status.TRUE.getKey())));
    }
}
