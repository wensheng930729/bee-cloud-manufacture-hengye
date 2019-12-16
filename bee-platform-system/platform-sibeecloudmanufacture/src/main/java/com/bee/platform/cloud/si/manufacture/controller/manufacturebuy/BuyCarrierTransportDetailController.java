package com.bee.platform.cloud.si.manufacture.controller.manufacturebuy;


import com.bee.platform.cloud.si.manufacture.dto.BuyTransportDetailDTO;
import com.bee.platform.cloud.si.manufacture.service.manufacturebuy.BuyCarrierTransportDetailService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
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
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;

/**
 * <p>
 * 承运方运输详情表(采购) 前端控制器
 * </p>
 *
 * @author qhwang
 * @since 2019-09-23
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "buyCarrierTransportDetail", tags = "采购-承运方运输车次详情相关接口")
@RequestMapping("/buyCarrierTransportDetail")
public class BuyCarrierTransportDetailController {

    @Autowired
    private UserInfoUtils userInfoUtils;

    @Autowired
    private BuyCarrierTransportDetailService carrierTransportDetailService;

    @PostMapping(value="/saveTransportSection")
    @ApiOperation(value="更新承运商车次信息",notes="更新承运商车次信息")
    public ResponseResult<ResCodeEnum> saveTransportSection(HttpServletRequest request,
                                                            @RequestBody() BuyTransportDetailDTO transportDetailDTO){
        // 获取当前用户信息
        String sysToken = WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request);
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return carrierTransportDetailService.saveTransportDetailByTransport(transportDetailDTO, userInfo);
    }

}

