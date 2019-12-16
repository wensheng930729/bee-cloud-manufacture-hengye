package com.bee.platform.cloud.si.manufacture.service.manufactureproduce;

import com.bee.platform.cloud.si.manufacture.dto.ProDeviceInspectionStatisticDTO;
import com.bee.platform.cloud.si.manufacture.dto.ProFurnaceDataDTO;
import com.bee.platform.cloud.si.manufacture.dto.ProFurnaceRateDTO;
import com.bee.platform.cloud.si.manufacture.rq.ProStatisticRQ;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResponseResult;

import java.util.List;

/**
 * @author xin.huang
 * @description 生产数据统计
 * @date 2019/10/18
 */
public interface ProStatisticService {

    /**
     * @descriptin 条件查询矿热炉回收率
     * @author xin.huang
     * @param rq
     * @param userInfo
     * @date 2019/10/18
     * @return
     */
    ResponseResult<List<ProFurnaceRateDTO>> findFurnaceRecoveryRate(ProStatisticRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * @descriptin 条件查询矿热炉产量
     * @author xin.huang
     * @param rq
     * @param userInfo
     * @date 2019/10/21
     * @return
     */
    ResponseResult<ProFurnaceDataDTO> findFurnaceProduction(ProStatisticRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * @descriptin 条件查询矿热炉产出质量
     * @author xin.huang
     * @param rq
     * @date 2019/10/21
     * @return
     */
    ResponseResult<ProFurnaceDataDTO> findProductSpecs(ProStatisticRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * @descriptin 条件查询原料消耗
     * @author xin.huang
     * @param rq
     * @date 2019/10/21
     * @return
     */
    ResponseResult<ProFurnaceDataDTO> findMaterialConsume(ProStatisticRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * @descriptin 条件查询原料吨耗
     * @author xin.huang
     * @param rq
     * @param userInfo
     * @date 2019/10/22
     * @return
     */
    ResponseResult<ProFurnaceDataDTO> findMaterialTonConsume(ProStatisticRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * @descriptin 条件查询矿热炉电力消耗
     * @author xin.huang
     * @param rq
     * @param userInfo
     * @date 2019/10/22
     * @return
     */
    ResponseResult<ProFurnaceDataDTO> findPowerConsume(ProStatisticRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * @descriptin 查询当天的吨电耗
     * @author xin.huang
     * @param userInfo
     * @date 2019/10/22
     * @return
     */
    ResponseResult<ProFurnaceDataDTO> findPowerTonConsume(ProStatisticRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * @descriptin 统计设备检修状况
     * @author xin.huang
     * @param userInfo
     * @date 2019/10/22
     * @return
     */
    ResponseResult<ProDeviceInspectionStatisticDTO> findStatisticDeviceInspection(AuthPlatformUserInfo userInfo);
}
