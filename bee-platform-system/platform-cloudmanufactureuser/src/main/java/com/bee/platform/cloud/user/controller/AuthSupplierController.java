package com.bee.platform.cloud.user.controller;


import com.bee.platform.cloud.user.dto.AuthSupplierDTO;
import com.bee.platform.cloud.user.dto.AuthSupplierNameDTO;
import com.bee.platform.cloud.user.dto.CarrierInfoDTO;
import com.bee.platform.cloud.user.rq.AuthSearchTypeListRQ;
import com.bee.platform.cloud.user.rq.AuthSupplierSaveRQ;
import com.bee.platform.cloud.user.rq.AuthSupplierSearchRQ;
import com.bee.platform.cloud.user.rq.AuthSupplierUpdateRQ;
import com.bee.platform.cloud.user.service.AuthCustomerOrSupplierService;
import com.bee.platform.common.annotation.NotIntercept;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.UserInfoUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;

/**
 * <p>
 * 供应商管理 前端控制器
 * </p>
 *
 * @author MP123
 * @since 2019-10-09
 */
@Slf4j
@CrossOrigin(origins = "*")
@Api(value = "Supplier", tags = "C-供应商管理")
@RestController
@RequestMapping("/supplier")
public class AuthSupplierController {




    @Autowired
    private AuthCustomerOrSupplierService customerOrSupplierService;

    @Autowired
    private UserInfoUtils userInfoUtils;


    @PostMapping("/searchSupplierList")
    @ApiOperation(value = "条件查询供应商列表")
    public ResponseResult<List<AuthSupplierDTO>> searchSupplierList(@RequestHeader("sysToken") String sysToken,@RequestBody AuthSupplierSearchRQ rq, Page page){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }

        return customerOrSupplierService.searchSupplierList(userInfo,rq,page);
    }


    @ApiOperation(value = "保存供应商信息")
    @PostMapping("/saveSupplier")
    public ResponseResult<Integer> saveSupplier(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid AuthSupplierSaveRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }


        Integer id = customerOrSupplierService.saveSupplier(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);

    }


    @ApiOperation(value = "修改供应商信息")
    @PostMapping("/updateSupplier")
    public ResponseResult<Integer> updateSupplier(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid AuthSupplierUpdateRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }


        Integer id = customerOrSupplierService.updateSupplier(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);

    }


    @ApiOperation(value = "根据id删除供应商信息")
    @DeleteMapping("/deleteSupplierById")
    public ResponseResult<Integer> deleteSupplierById(@RequestHeader("sysToken") String sysToken, @RequestParam("id") Integer id) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(id)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        customerOrSupplierService.deleteSupplierById(userInfo, id);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }



    @GetMapping("/getSupplierById")
    @ApiOperation(value = "根据id查询供应商详情")
    public ResponseResult<AuthSupplierDTO> getSupplierById(@RequestHeader("sysToken") String sysToken, @RequestParam("id") Integer id){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        AuthSupplierDTO dto = customerOrSupplierService.getSupplierById(userInfo,id);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }

    @GetMapping("/getCarrierInfoList")
    @ApiOperation(value = "查询用户所属公司的承运商信息")
    public ResponseResult<List<CarrierInfoDTO>> getCarrierInfoList(@RequestHeader("sysToken") String sysToken){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return customerOrSupplierService.getCarrierInfoList(userInfo);
    }


    @PostMapping("/getSupplierListByCategory")
    @ApiOperation(value = "查询用户公司下的供应商列表（供应商类别（0核心供应商 1战略供应商  2储备供应商）空为全部）")
    public ResponseResult<List<AuthSupplierDTO>> getSupplierListByCategory(@RequestHeader("sysToken") String sysToken,@RequestBody AuthSearchTypeListRQ rq){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        List<AuthSupplierDTO> dto =  customerOrSupplierService.getSupplierListByCategory(userInfo,rq.getTypes());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }


    @NotIntercept
    @GetMapping("/getSupplierNameById")
    @ApiOperation(value = "根据id查询供应商名称")
    public ResponseResult<AuthSupplierNameDTO> getSupplierNameById(@RequestParam("id") Integer id){

        AuthSupplierNameDTO dto = customerOrSupplierService.getSupplierNameById(id);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }
}

