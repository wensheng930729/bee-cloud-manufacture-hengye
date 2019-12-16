package com.bee.platform.cloud.si.manufacture.controller.factoryconfig;


import com.bee.platform.cloud.si.manufacture.dto.ConfigProductDTO;
import com.bee.platform.cloud.si.manufacture.dto.ConfigProductListDTO;
import com.bee.platform.cloud.si.manufacture.dto.ConfigProductTestAttributeInDTO;
import com.bee.platform.cloud.si.manufacture.dto.ConfigProductTestAttributeOutDTO;
import com.bee.platform.cloud.si.manufacture.rq.ConfigProductSaveRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigProductSearchRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigProductTestItemUpdateRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigProductUpdateRQ;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigProductService;
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
 * 产品档案 前端控制器
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "configProduct", tags = "C-产品管理相关接口")
@RequestMapping("/configProduct")
public class ConfigProductController {


    @Autowired
    private ConfigProductService configProductService;

    @Autowired
    private UserInfoUtils userInfoUtils;

    @PostMapping("/searchProductList")
    @ApiOperation(value = "条件查询产品列表")
    public ResponseResult<List<ConfigProductListDTO>> searchProductList(@RequestHeader("sysToken") String sysToken, @RequestBody ConfigProductSearchRQ rq, Page page){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return configProductService.searchProductList(userInfo,rq,page);
    }


    @ApiOperation(value = "保存产品信息")
    @PostMapping("/saveProduct")
    public ResponseResult<Integer> saveProduct(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid ConfigProductSaveRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }


        Integer id = configProductService.saveProduct(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);

    }


    @ApiOperation(value = "修改产品信息")
    @PostMapping("/updateProduct")
    public ResponseResult<Integer> updateProduct(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid ConfigProductUpdateRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }


        Integer id = configProductService.updateProduct(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);

    }


    @ApiOperation(value = "根据id删除产品信息")
    @DeleteMapping("/deleteProductById")
    public ResponseResult<Integer> deleteProductById(@RequestHeader("sysToken") String sysToken, @RequestParam("id") Integer id) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(id)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        configProductService.deleteProductById(userInfo, id);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }


    @ApiOperation(value = "根据id查看产品详情信息")
    @GetMapping("/getProductById")
    public ResponseResult<ConfigProductDTO> getProductById(@RequestHeader("sysToken") String sysToken, @RequestParam("id") Integer id) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(id)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, configProductService.getProductById(userInfo, id));
    }



    @ApiOperation(value = "修改产品化验配置信息")
    @PostMapping("/updateTestItem")
    public ResponseResult<Integer> updateTestItem(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid ConfigProductTestItemUpdateRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }


        Integer id = configProductService.updateTestItem(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);

    }



    @ApiOperation(value = "根据产品id查询产品化验结果项")
    @GetMapping("/getTestAttributeOutByProductId/{productId}")
    public ResponseResult<List<ConfigProductTestAttributeOutDTO>> getTestAttributeOutByProductId(@RequestHeader("sysToken") String sysToken, @PathVariable(value = "productId") Integer productId) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(productId)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, configProductService.getProductAttributeOutByProductId(productId,userInfo));
    }


    @ApiOperation(value = "根据产品id查询产品化验输入项")
    @GetMapping("/getTestAttributeInByProductId/{productId}")
    public ResponseResult<List<ConfigProductTestAttributeInDTO>> getTestAttributeInByProductId(@RequestHeader("sysToken") String sysToken, @PathVariable(value = "productId") Integer productId) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(productId)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, configProductService.getProductAttributeInByProductId(productId,userInfo));
    }



    @GetMapping("/getProductListByCategory")
    @ApiOperation(value = "根据产品类型查询产品列表(产品类别 0 全部 1 主料 2 辅料 3 成品 4 用户定义的类别  5 包含(主料 辅料)  6 包含(主料  成品) 7包含(辅料  成品) 8包含(主料 辅料 成品)")
    public ResponseResult<List<ConfigProductListDTO>> getProductListByCategory(@RequestHeader("sysToken") String sysToken,@RequestParam(value = "category") Integer category){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        List<ConfigProductListDTO> dto = configProductService.getProductListByCategory(userInfo,category);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }

}

