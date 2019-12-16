package com.bee.platform.cloud.si.manufacture.service.BigScreenStatistic;

import com.bee.platform.cloud.si.manufacture.dto.ActualPfDTO;
import com.bee.platform.cloud.si.manufacture.dto.BigScreenProductionDTO;
import com.bee.platform.cloud.si.manufacture.dto.CurrentDTO;
import com.bee.platform.cloud.si.manufacture.dto.ElectricityDataDTO;
import com.bee.platform.cloud.si.manufacture.dto.ElectricityUseDTO;
import com.bee.platform.cloud.si.manufacture.dto.EnterLeaveFactoryDTO;
import com.bee.platform.cloud.si.manufacture.dto.ProFurnaceRateDTO;
import com.bee.platform.cloud.si.manufacture.entity.ZElecMeterDataVO;
import com.bee.platform.cloud.si.manufacture.rq.ProStatisticRQ;
import com.bee.platform.cloud.si.manufacture.dto.ProDeviceInspectionStatisticDTO;
import com.bee.platform.cloud.si.manufacture.dto.ProFurnaceDataDTO;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResponseResult;

import java.util.List;

/**
 * @author xin.huang
 * @description 企业电子大屏数据展示相关接口
 * @date 2019/11/12
 */
public interface BigScreenStatisticService {

    /**
     * @descriptin 查询矿热炉当日产量
     * @author xin.huang
     * @param userInfo
     * @date 2019/11/12
     * @return
     */
    ResponseResult<List<BigScreenProductionDTO>> findFurnaceProduction(AuthPlatformUserInfo userInfo, Integer data);

    /**
     * 查询当日出厂数据
     * @param userInfo
     * @return
     */
	ResponseResult<List<EnterLeaveFactoryDTO>> findBuyFactoryData(AuthPlatformUserInfo userInfo);

	/**
	 * 查询当日入场数据
	 * @param userInfo
	 * @return
	 */
	ResponseResult<List<EnterLeaveFactoryDTO>> findSaleFactoryData(AuthPlatformUserInfo userInfo);
     
	/* @descriptin 查询矿热炉当日各成品产量
     * @author xin.huang
     * @param userInfo
     * @date 2019/11/12
     * @return
     */
    ResponseResult<ProFurnaceDataDTO> findFurnaceFinishProduction(AuthPlatformUserInfo userInfo, Integer data);

    /**
     * @descriptin 统计设备检修状况
     * @author xin.huang
     * @param userInfo
     * @date 2019/11/12
     * @return
     */
    ResponseResult<ProDeviceInspectionStatisticDTO> findStatisticDeviceInspection(AuthPlatformUserInfo userInfo, Integer data);

    /**
     * @descriptin 查询当前时间下的原料消耗
     * @author xin.huang
     * @param userInfo
     * @date 2019/11/12
     * @return
     */
    ResponseResult<ProFurnaceDataDTO> findCurrentMaterialConsume(AuthPlatformUserInfo userInfo, Integer data);

    /**
     * 查询热炉三相电流信息
     * @param zElecMeterDataVO 
     * @param userInfo
     * @return
     */
	ResponseResult<List<CurrentDTO>> findElectricityByDevice(String factoryId,String deviceId,
			Long startTime,Long endTime);

	/**
	 * 查询热炉电量使用信息
	 * @param factoryId
	 * @param startTime
	 * @param endTime
	 * @return
	 */
	ResponseResult<List<ElectricityUseDTO>> findTotalElectricityUseInfo(String factoryId, Long startTime,
			Long endTime);
	
	
    /**
     * 查询实时得总功率因素
     * @param zElecMeterDataVO 
     * @param userInfo
     * @return
     */
	List<ActualPfDTO> findActualPfc(String factoryId, Long endTime);
}
