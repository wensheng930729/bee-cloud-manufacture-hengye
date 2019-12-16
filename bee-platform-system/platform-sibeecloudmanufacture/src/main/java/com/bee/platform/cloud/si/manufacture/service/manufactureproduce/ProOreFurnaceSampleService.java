package com.bee.platform.cloud.si.manufacture.service.manufactureproduce;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.entity.ProOreFurnaceSample;
import com.bee.platform.cloud.si.manufacture.rq.ProOreFurnaceSampleRQ;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResponseResult;

import java.util.Date;

/**
 * <p>
 * 服务类
 * </p>
 *
 * @author huangxin123
 * @since 2019-10-08
 */
public interface ProOreFurnaceSampleService extends IService<ProOreFurnaceSample> {

    ResponseResult noticeSample(ProOreFurnaceSampleRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * 查询是否已经通知取样
     *
     * @param openTime 开班时间
     * @return
     */
    int getNoticeSampleCount(Integer furnaceId, Integer shift, Integer furnaceBatch, Date openTime);
}
