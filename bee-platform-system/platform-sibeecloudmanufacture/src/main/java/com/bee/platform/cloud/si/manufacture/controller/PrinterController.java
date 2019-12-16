package com.bee.platform.cloud.si.manufacture.controller;


import com.bee.platform.cloud.si.manufacture.rq.PrintRq;
import com.bee.platform.cloud.si.manufacture.service.ConfigPrinterTokenService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.UserInfoUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;


/**
 * <p>
 * 打印机相关接口
 * </p>
 *@author
 *@date
 */
@RestController
@CrossOrigin(origins = "*")
@RequestMapping("/print")
@Api(value = "print", tags = "打印机相关接口")
public class PrinterController {

    @Autowired
    private UserInfoUtils userInfoUtils;
    @Autowired
    private ConfigPrinterTokenService printerTokenService;

    @ApiOperation(value = "磅单打印")
    @PostMapping("/printPoundSheet")
    public ResponseResult saveProduct(@RequestHeader("sysToken") String sysToken, @RequestBody PrintRq rq){
        //用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return printerTokenService.printPoundSheet(userInfo,rq);

    }

}

