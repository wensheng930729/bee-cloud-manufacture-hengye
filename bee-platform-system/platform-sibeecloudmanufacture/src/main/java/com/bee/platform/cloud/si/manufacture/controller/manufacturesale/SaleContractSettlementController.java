package com.bee.platform.cloud.si.manufacture.controller.manufacturesale;


import com.bee.platform.cloud.si.manufacture.rq.SaleContractSettlementRQ;
import com.bee.platform.cloud.si.manufacture.service.manufacturesale.SaleContractSettlementService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
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

/**
 * <p>
 * 销售合同结算表 前端控制器
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-09-26
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "saleContractSettlement", tags = "销售-合同结算相关接口")
@RequestMapping("/saleContractSettlement")
public class SaleContractSettlementController {

    @Autowired
    private UserInfoUtils userInfoUtils;

    @Autowired
    private SaleContractSettlementService saleContractSettlementService;

    @PostMapping(value="/saveContractSettleInfo")
    @ApiOperation(value="保存合同结算情况",notes="保存合同结算情况")
    public ResponseResult<ResCodeEnum> saveContractSettleInfo(@RequestHeader(value = "sysToken")String sysToken,
                                                              @RequestBody() @Valid SaleContractSettlementRQ contractSettlementRQ){
        // 获取当前用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return saleContractSettlementService.saveContractSettleInfo(contractSettlementRQ, userInfo);
    }

}

