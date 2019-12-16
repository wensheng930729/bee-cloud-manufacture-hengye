package com.bee.platform.cloud.si.manufacture.service.manufactureproduce;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.dto.DeviceInspectionDTO;
import com.bee.platform.cloud.si.manufacture.entity.ProDeviceInspection;
import com.bee.platform.cloud.si.manufacture.rq.ProDeviceInspectionConfirmRQ;
import com.bee.platform.cloud.si.manufacture.rq.ProDeviceInspectionRQ;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResponseResult;

/**
 * <p>
 *  服务类
 * </p>
 *
 * @author huangxin123
 * @since 2019-09-25
 */
public interface ProDeviceInspectionService extends IService<ProDeviceInspection> {

    ResponseResult add(ProDeviceInspectionRQ rq, AuthPlatformUserInfo userInfo);

    ResponseResult<DeviceInspectionDTO> findDeviceInspectionList(AuthPlatformUserInfo userInfo);

    ResponseResult confirmDeviceInspection(AuthPlatformUserInfo userInfo, ProDeviceInspectionConfirmRQ rq);

}
