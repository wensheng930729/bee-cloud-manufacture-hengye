package com.bee.platform.cloud.si.manufacture.controller.manufacturesale;


import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.si.manufacture.dto.SaleCarrierTransportDTO;
import com.bee.platform.cloud.si.manufacture.dto.SaleTransportReportDTO;
import com.bee.platform.cloud.si.manufacture.rq.SaleCarrierTransportSearchRQ;
import com.bee.platform.cloud.si.manufacture.service.manufacturesale.SaleCarrierTransportService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.common.utils.UserInfoUtils;
import com.bee.platform.common.utils.WebUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.util.List;

/**
 * <p>
 * 运输段承运方表(销售) 前端控制器
 * </p>
 *
 * @author qhwang
 * @since 2019-09-26
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "saleCarrierTransport", tags = "销售-运输段承运方相关接口")
@RequestMapping("/saleCarrierTransport")
public class SaleCarrierTransportController {

    @Autowired
    private UserInfoUtils userInfoUtils;

    @Autowired
    private SaleCarrierTransportService carrierTransportService;

    @PostMapping(value = "/saveCarrierTransport")
    @ApiOperation(value = "保存运输段承运方信息", notes = "保存运输段承运方信息")
    public ResponseResult<ResCodeEnum> saveCarrierTransport(HttpServletRequest request,
                                                            @RequestBody() SaleCarrierTransportDTO carrierTransportDTO) {
        // 获取当前用户信息
        String sysToken = WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request);
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return carrierTransportService.saveCarrierTransport(carrierTransportDTO, userInfo);
    }

}

