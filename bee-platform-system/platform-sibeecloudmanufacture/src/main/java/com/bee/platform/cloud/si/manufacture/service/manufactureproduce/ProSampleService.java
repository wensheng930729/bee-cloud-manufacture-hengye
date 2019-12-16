package com.bee.platform.cloud.si.manufacture.service.manufactureproduce;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.dto.SampleAssayDetailDTO;
import com.bee.platform.cloud.si.manufacture.entity.ProSample;
import com.bee.platform.cloud.si.manufacture.rq.*;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;

import java.util.Date;
import java.util.List;

/**
 * <p>
 * 生产样品表 服务类
 * </p>
 *
 * @author liliang123
 * @since 2019-09-27
 */
public interface ProSampleService extends IService<ProSample> {
    /**
     * 保存取样信息
     */
    public ResponseResult<ResCodeEnum> saveSample(SaveSampleProRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * 完成取样
     */
    public ResponseResult<ResCodeEnum> finishSample(FinishSampleProRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * 化验人员开始化验样品
     */
    public ResponseResult<ResCodeEnum> startAssaySample(SampleAssayStartRQ rq, Object o, AuthPlatformUserInfo userInfo);

    /**
     * 保存样品化验结果
     */
    public ResponseResult<ResCodeEnum> saveAssayResult(SampleAssayResultSaveRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * 化验人员弃用样品
     */
    public ResponseResult<ResCodeEnum> abandonSample(SampleAssayAbandonRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * 根据样品code获取样品详情
     *
     * @param o
     * @param sampleCode
     * @return
     */
    public ResponseResult<SampleAssayDetailDTO> getSampleAssayDetailByCode(Object o, String sampleCode);

    /**
     * 查询是否取样 且未弃样 生产每个炉次只能取样一次
     *
     * @param furnaceId    炉号
     * @param shift        班次
     * @param furnaceBatch 炉次
     * @param openTime     开班时间
     * @param userInfo
     * @return
     */
    public List<ProSample> checkIfSample(Integer furnaceId, Integer shift, Integer furnaceBatch, Date openTime, AuthPlatformUserInfo userInfo);

}

