package com.bee.platform.cloud.si.manufacture.controller.manufacturebigscreen;

import com.bee.platform.cloud.si.manufacture.dto.BigScreenProductionDTO;
import com.bee.platform.cloud.si.manufacture.dto.CurrentDTO;
import com.bee.platform.cloud.si.manufacture.dto.ElectricityDataDTO;
import com.bee.platform.cloud.si.manufacture.dto.ElectricityUseDTO;
import com.bee.platform.cloud.si.manufacture.dto.EnterLeaveFactoryDTO;
import com.bee.platform.cloud.si.manufacture.dto.ProDeviceInspectionStatisticDTO;
import com.bee.platform.cloud.si.manufacture.dto.ProFurnaceDataDTO;
import com.bee.platform.cloud.si.manufacture.entity.BigScreenProductionExcel;
import com.bee.platform.cloud.si.manufacture.entity.ZElecMeterData;
import com.bee.platform.cloud.si.manufacture.entity.ZElecMeterDataVO;
import com.bee.platform.cloud.si.manufacture.service.ZElecMeterDataService;
import com.bee.platform.cloud.si.manufacture.service.BigScreenStatistic.BigScreenStatisticService;
import com.bee.platform.cloud.si.manufacture.utils.ExcelUtils;
import com.bee.platform.cloud.si.manufacture.utils.JsonResult;
import com.bee.platform.cloud.si.manufacture.utils.ResultUtils;
import com.bee.platform.common.annotation.NotIntercept;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.UserInfoUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletResponse;
import java.util.ArrayList;
import java.util.List;
import java.util.TimeZone;

/**
 * @author xin.huang
 * @description
 * @date 2019/11/12
 */
@RestController
@RequestMapping("/BigScreenStatistic")
@CrossOrigin(origins = "*")
@Api(value = "BigScreenStatistic", tags = "企业电子大屏数据展示相关接口")
public class BigScreenStatisticController {

    @Autowired
    private UserInfoUtils userInfoUtils;

    @Autowired
    private BigScreenStatisticService bigScreenStatisticService;
    
    @Autowired
    private ZElecMeterDataService zElecMeterDataService;

    @NotIntercept
    @GetMapping("/production")
    @ApiOperation(value = "查询矿热炉产量")
    @ApiImplicitParam(name = "data", value = "0:模拟数据，1真实数据", required = true)
    public ResponseResult<List<BigScreenProductionDTO>> findFurnaceProduction(Integer data) {
    	AuthPlatformUserInfo userInfo = new AuthPlatformUserInfo();
//    	// 测试用
//    	userInfo.setOrgId(1500);
//    	userInfo.setFactoryId(1);
    	
    	// 正式用
    	userInfo.setOrgId(1501);
    	userInfo.setFactoryId(2);
        return bigScreenStatisticService.findFurnaceProduction(userInfo, data);
    }
    
    @NotIntercept
    @GetMapping("/leaveFactory")
    @ApiOperation(value = "实时出厂数据")
    public ResponseResult<List<EnterLeaveFactoryDTO>> findLeaveFactoryData() {
    	AuthPlatformUserInfo userInfo = new AuthPlatformUserInfo();
//    	// 测试用
//    	userInfo.setOrgId(1500);
//    	userInfo.setFactoryId(1);
    	
    	// 正式用
    	userInfo.setOrgId(1501);
    	userInfo.setFactoryId(2);
        return bigScreenStatisticService.findSaleFactoryData(userInfo);
    }
    
    @NotIntercept
    @GetMapping("/EnterFactory")
    @ApiOperation(value = "实时入厂数据")
    public ResponseResult<List<EnterLeaveFactoryDTO>> findEnterFactoryData() {
    	AuthPlatformUserInfo userInfo = new AuthPlatformUserInfo();
//    	// 测试用
//    	userInfo.setOrgId(1500);
//    	userInfo.setFactoryId(1);
    	
    	// 正式用
    	userInfo.setOrgId(1501);
    	userInfo.setFactoryId(2);
        return bigScreenStatisticService.findBuyFactoryData(userInfo);
    }

    @NotIntercept
    @GetMapping("/deviceInspection")
    @ApiOperation(value = "统计设备检修状况")
    @ApiImplicitParam(name = "data", value = "0:模拟数据，1真实数据", required = true)
    public ResponseResult<ProDeviceInspectionStatisticDTO> findStatisticDeviceInspection(Integer data) {
    	AuthPlatformUserInfo userInfo = new AuthPlatformUserInfo();
//    	// 测试用
//    	userInfo.setOrgId(1500);
//    	userInfo.setFactoryId(1);

    	// 正式用
    	userInfo.setOrgId(1501);
    	userInfo.setFactoryId(2);
        return bigScreenStatisticService.findStatisticDeviceInspection(userInfo, data);
    }

    @NotIntercept
    @GetMapping("/finishProduction")
    @ApiImplicitParam(name = "data", value = "0:模拟数据，1真实数据", required = true)
    @ApiOperation(value = "查询矿热炉各成品产量")
    public ResponseResult<ProFurnaceDataDTO> findFurnaceFinishProduction(Integer data) {
        AuthPlatformUserInfo userInfo = new AuthPlatformUserInfo();
//        // 测试用
//        userInfo.setOrgId(1500);
//        userInfo.setFactoryId(1);

    	// 正式用
    	userInfo.setOrgId(1501);
    	userInfo.setFactoryId(2);
        return bigScreenStatisticService.findFurnaceFinishProduction(userInfo, data);
    }

    @NotIntercept
    @GetMapping("/materialConsume")
    @ApiOperation(value = "查询原料消耗")
    @ApiImplicitParam(name = "data", value = "0:模拟数据，1真实数据", required = true)
    public ResponseResult<ProFurnaceDataDTO> findCurrentMaterialConsume(Integer data) {
    	AuthPlatformUserInfo userInfo = new AuthPlatformUserInfo();
//    	// 测试用
//    	userInfo.setOrgId(1500);
//    	userInfo.setFactoryId(1);
    	
    	// 正式用
    	userInfo.setOrgId(1501);
    	userInfo.setFactoryId(2);
        return bigScreenStatisticService.findCurrentMaterialConsume(userInfo, data);
    }
    
    @NotIntercept
    @GetMapping("/findElectricityByDeviceOne")
    @ApiOperation(value = "查询热炉1的三相电流数据")
    public ResponseResult<List<CurrentDTO>> findElectricityByDeviceOne() {
//		// 测试条件
//		String  deviceId = "Device1";
//		String factoryId = "60";
//        Long endTime = 1551110400l - 59;
//        Long startTime = endTime - 24 * 60 * 60;
        
      // 正式条件
		String  deviceId = "Device1";
		String factoryId = "78";
      Long endTime = System.currentTimeMillis()/1000  - 59;
		Long startTime= endTime - 24 * 60 * 60;
        return bigScreenStatisticService.findElectricityByDevice(factoryId,deviceId,startTime,endTime);
    }
    
    @NotIntercept
    @GetMapping("/findElectricityByDeviceTwo")
    @ApiOperation(value = "查询热炉2的三相电流数据")
    public ResponseResult<List<CurrentDTO>> findElectricityByDeviceTwo() {
//		// 测试条件
//		String  deviceId = "Device2";
//		String factoryId = "60";
//        Long endTime = 1551110400l  - 59;
//        Long startTime = endTime - 24 * 60 * 60;
        
      // 正式条件
		String  deviceId = "Device2";
		String factoryId = "78";
      Long endTime = System.currentTimeMillis()/1000  - 59;
		Long startTime= endTime - 24 * 60 * 60;
        return bigScreenStatisticService.findElectricityByDevice(factoryId,deviceId,startTime,endTime);
    }
    
    @NotIntercept
    @GetMapping("/findElectricityByDeviceThree")
    @ApiOperation(value = "查询热炉3的三相电流数据")
    public ResponseResult<List<CurrentDTO>> findElectricityByDeviceThree() {
//		// 测试条件
//		String  deviceId = "Device3";
//		String factoryId = "60";
//        Long endTime = 1551110400l  - 59;
//        Long startTime = endTime - 24 * 60 * 60;
        
      // 正式条件
		String  deviceId = "Device3";
		String factoryId = "78";
      Long endTime = System.currentTimeMillis()/1000  - 59;
		Long startTime= endTime - 24 * 60 * 60;
        return bigScreenStatisticService.findElectricityByDevice(factoryId,deviceId,startTime,endTime);
    }
    
    @NotIntercept
    @GetMapping("/findElectricityByDeviceFour")
    @ApiOperation(value = "查询热炉4的三相电流数据")
    public ResponseResult<List<CurrentDTO>> findElectricityByDeviceFour() {
//		// 测试条件
//		String  deviceId = "Device4";
//		String factoryId = "60";
//        Long endTime = 1551110400l  - 59;
//        Long startTime = endTime - 24 * 60 * 60;
        
      // 正式条件
		String  deviceId = "Device4";
		String factoryId = "78";
      Long endTime = System.currentTimeMillis()/1000  - 59;
		Long startTime= endTime - 24 * 60 * 60;
        return bigScreenStatisticService.findElectricityByDevice(factoryId,deviceId,startTime,endTime);
    }
    
    
    @NotIntercept
    @GetMapping("/findTotalElectricityUseInfo")
    @ApiOperation(value = "查询各热炉的电量使用情况")
    public ResponseResult<List<ElectricityUseDTO>> findTotalElectricityUseInfo() {
//		// 测试条件
//		String factoryId = "60";
//        Long endTime = 1551110400l;
//        Long startTime = endTime - 24 * 60 * 60 * 6;
        
      // 正式条件
	    String factoryId = "78";
	    Long nowTime =System.currentTimeMillis();
    	Long todayStartTime =nowTime - ((nowTime + TimeZone.getDefault().getRawOffset()) % (24 * 60 * 60 * 1000L));
      // 转化成秒数
    	Long todayZeroTime = todayStartTime/1000;
      // 前一周的0点时间戳
    	Long startTime= todayZeroTime - 24 * 60 * 60 * 6;
      Long endTime =System.currentTimeMillis()/1000;

        return bigScreenStatisticService.findTotalElectricityUseInfo(factoryId,startTime,endTime);
    }
    
    
//    /**
//     * 按时间查询所有电表数据
//     * @param zElecMeterDataVO factoryId，deviceId ,startTime,endTime,currentToken（当前登录用户的token）
//     * @return
//     */
//    @NotIntercept
//    @GetMapping("/all")
//    @ApiOperation(value = "电流数据")
//    public JsonResult findByDuration(@RequestBody ZElecMeterDataVO zElecMeterDataVO) {
//        List<ZElecMeterData> zMeterData = zElecMeterDataService.findByDuration(zElecMeterDataVO);
//        return ResultUtils.success(zMeterData);
//    }

    @NotIntercept
    @GetMapping("/exportAmmeter")
    @ApiOperation(value = "导出电表")
    public void exportAmmeter(HttpServletResponse response) {
        System.out.println(1);
//        模拟从数据库获取需要导出的数据
        List<BigScreenProductionExcel> list = new ArrayList<>();
//         导出操作
        ExcelUtils.exportExcel(list, "导出产量信息", "产量分析", BigScreenProductionExcel.class, "Production.xls", response);

    }
    
}
