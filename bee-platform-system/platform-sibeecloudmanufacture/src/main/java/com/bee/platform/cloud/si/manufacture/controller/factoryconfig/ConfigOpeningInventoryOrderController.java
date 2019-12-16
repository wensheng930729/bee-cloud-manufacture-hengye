package com.bee.platform.cloud.si.manufacture.controller.factoryconfig;


import com.bee.platform.cloud.si.manufacture.dto.ConfigOpeningInventoryOrderDTO;
import com.bee.platform.cloud.si.manufacture.dto.ConfigOpeningInventorySearchDTO;
import com.bee.platform.cloud.si.manufacture.rq.ConfigOpeningInventoryOrderRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigOpeningInventorySearchRQ;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigOpeningInventoryOrderService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.UserInfoUtils;
import com.bee.platform.common.utils.WebUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.*;

import springfox.documentation.annotations.ApiIgnore;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;
import java.util.List;

/**
 * <p>
 * 期初库存主表 前端控制器
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "configOpeningInventoryOrder", tags = "C-期初库存主表相关接口")
@RequestMapping("/configOpeningInventoryOrder")
public class ConfigOpeningInventoryOrderController {


    @Autowired
    private UserInfoUtils userInfoUtils;

    @Autowired
    private ConfigOpeningInventoryOrderService openingInventoryOrderService;



    @ApiOperation(value = "生成订单编号", notes = "生成订单编号")
    @GetMapping("/generateOrderId")
    public ResponseResult generateCode() {

        String code = openingInventoryOrderService.generateCode();

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, (Object)code);

    }


    @PostMapping("/searchOpeningInventoryByCondition")
    @ApiOperation(value = "条件查询期初库存信息列表")
    public ResponseResult<List<ConfigOpeningInventorySearchDTO>> searchOpeningInventoryByCondition(@RequestHeader("sysToken") String sysToken, @RequestBody ConfigOpeningInventorySearchRQ rq, Page page) {

        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }

        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        return openingInventoryOrderService.searchOpeningInventoryByCondition(userInfo,rq, page);
    }


    @ApiOperation(value = "根据id查看期初库存详情")
    @GetMapping("/getOpeningInventoryById")
    public ResponseResult<ConfigOpeningInventoryOrderDTO> getOpeningInventoryById(@RequestParam("id") Integer id) {
        if (ObjectUtils.isEmpty(id)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        ConfigOpeningInventoryOrderDTO dto = openingInventoryOrderService.getOpeningInventoryById(id);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);

    }



    @PostMapping("/saveOpeningInventoryOrder")
    @ApiOperation(value = "保存期初库存")
    public ResponseResult<Integer> saveOpeningInventoryOrder(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid ConfigOpeningInventoryOrderRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        Integer id = openingInventoryOrderService.saveOpeningInventoryOrder(userInfo, rq);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }

}

