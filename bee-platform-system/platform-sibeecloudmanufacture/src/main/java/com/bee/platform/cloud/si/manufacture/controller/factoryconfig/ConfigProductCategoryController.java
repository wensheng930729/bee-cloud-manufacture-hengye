package com.bee.platform.cloud.si.manufacture.controller.factoryconfig;


import com.bee.platform.cloud.si.manufacture.dto.ConfigProductCategoryDTO;
import com.bee.platform.cloud.si.manufacture.rq.ConfigProductCategorySaveRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigProductCategorySearchRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigProductCategoryUpdateRQ;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigProductCategoryService;
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
 * 产品类别 前端控制器
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "configProductCategory", tags = "C-产品类别相关接口")
@RequestMapping("/configProductCategory")
public class ConfigProductCategoryController {


    @Autowired
    private ConfigProductCategoryService configProductCategoryService;

    @Autowired
    private UserInfoUtils userInfoUtils;

    @PostMapping("/searchProductCategoryList")
    @ApiOperation(value = "条件查询产品类别列表")
    public ResponseResult<List<ConfigProductCategoryDTO>> searchProductCategoryList(@RequestHeader("sysToken") String sysToken,@RequestBody ConfigProductCategorySearchRQ rq, Page page){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return configProductCategoryService.searchProductCategoryList(userInfo,rq,page);
    }



    @GetMapping("/getProductCategoryList")
    @ApiOperation(value = "获取产品类别列表--新增产品下拉列表使用")
    public ResponseResult<List<ConfigProductCategoryDTO>> getProductCategoryList(@RequestHeader("sysToken") String sysToken){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        List<ConfigProductCategoryDTO> dto = configProductCategoryService.getProductCategoryList(userInfo);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }

    @ApiOperation(value = "保存产品类别信息")
    @PostMapping("/saveProductCategory")
    public ResponseResult<Integer> saveProductCategory(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid ConfigProductCategorySaveRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }


        Integer id = configProductCategoryService.saveProductCategory(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);

    }


    @ApiOperation(value = "修改产品类别信息")
    @PostMapping("/updateProductCategory")
    public ResponseResult<Integer> updateProductCategory(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid ConfigProductCategoryUpdateRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }


        Integer id = configProductCategoryService.updateProductCategory(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);

    }


    @ApiOperation(value = "根据id删除产品类别信息")
    @DeleteMapping("/deleteProductCategoryById")
    public ResponseResult<Integer> deleteProductCategoryById(@RequestHeader("sysToken") String sysToken, @RequestParam("id") Integer id) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(id)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        configProductCategoryService.deleteProductCategoryById(userInfo, id);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }



    @GetMapping("/getProductCategoryById")
    @ApiOperation(value = "根据id查询产品类别详情")
    public ResponseResult<ConfigProductCategoryDTO> getProductCategoryById(@RequestHeader("sysToken") String sysToken, @RequestParam("id") Integer id){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        ConfigProductCategoryDTO dto = configProductCategoryService.getProductCategoryById(userInfo,id);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }
}

