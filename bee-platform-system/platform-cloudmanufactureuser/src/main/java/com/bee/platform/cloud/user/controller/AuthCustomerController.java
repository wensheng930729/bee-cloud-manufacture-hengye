package com.bee.platform.cloud.user.controller;


import com.bee.platform.cloud.user.dto.AuthCustomerDTO;
import com.bee.platform.cloud.user.dto.AuthCustomerNameDTO;
import com.bee.platform.cloud.user.rq.AuthCustomerSaveRQ;
import com.bee.platform.cloud.user.rq.AuthCustomerSearchRQ;
import com.bee.platform.cloud.user.rq.AuthCustomerUpdateRQ;
import com.bee.platform.cloud.user.rq.AuthSearchTypeListRQ;
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
 * 客户管理 前端控制器
 * </p>
 *
 * @author MP123
 * @since 2019-10-09
 */
@Slf4j
@CrossOrigin(origins = "*")
@Api(value = "Customer", tags = "C-客户管理")
@RestController
@RequestMapping("/customer")
public class AuthCustomerController {


    @Autowired
    private AuthCustomerOrSupplierService customerOrSupplierService;

    @Autowired
    private UserInfoUtils userInfoUtils;

    @PostMapping("/searchCustomerList")
    @ApiOperation(value = "条件查询客户管理列表")
    public ResponseResult<List<AuthCustomerDTO>> searchCustomerList(@RequestHeader("sysToken") String sysToken,@RequestBody AuthCustomerSearchRQ rq, Page page){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return customerOrSupplierService.searchCustomerList(userInfo,rq,page);
    }


    @ApiOperation(value = "保存客户管理信息")
    @PostMapping("/saveCustomer")
    public ResponseResult<Integer> saveCustomer(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid AuthCustomerSaveRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }


        Integer id = customerOrSupplierService.saveCustomer(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);

    }


    @ApiOperation(value = "修改客户管理信息")
    @PostMapping("/updateCustomer")
    public ResponseResult<Integer> updateCustomer(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid AuthCustomerUpdateRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }


        Integer id = customerOrSupplierService.updateCustomer(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);

    }


    @ApiOperation(value = "根据id删除客户管理信息")
    @DeleteMapping("/deleteCustomerById")
    public ResponseResult<Integer> deleteCustomerById(@RequestHeader("sysToken") String sysToken, @RequestParam("id") Integer id) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(id)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        customerOrSupplierService.deleteCustomerById(userInfo, id);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }

    @GetMapping("/getCustomerById")
    @ApiOperation(value = "根据id查询客户详情")
    public ResponseResult<AuthCustomerDTO> getCustomerById(@RequestHeader("sysToken") String sysToken, @RequestParam("id") Integer id){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        AuthCustomerDTO dto = customerOrSupplierService.getCustomerById(userInfo,id);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }




    @PostMapping("/getCustomerListByCategory")
    @ApiOperation(value = "查询用户公司下的客户列表（客户类别（0核心客户 1战略客户  2一般客户）空为全部）")
    public ResponseResult<List<AuthCustomerDTO>> getCustomerListByType(@RequestHeader("sysToken") String sysToken,@RequestBody AuthSearchTypeListRQ rq){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        List<AuthCustomerDTO> dto =  customerOrSupplierService.getCustomerListByType(userInfo,rq.getTypes());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }


    @NotIntercept
    @GetMapping("/getCustomerNameById")
    @ApiOperation(value = "根据id查询客户名称")
    public ResponseResult<AuthCustomerNameDTO> getCustomerNameById(@RequestParam("id") Integer id){

        AuthCustomerNameDTO dto = customerOrSupplierService.getCustomerNameById(id);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }




}

