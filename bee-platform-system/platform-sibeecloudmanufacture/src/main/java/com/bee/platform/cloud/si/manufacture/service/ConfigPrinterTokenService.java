package com.bee.platform.cloud.si.manufacture.service;

import com.bee.platform.cloud.si.manufacture.entity.ConfigPrinterToken;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.rq.PrintRq;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResponseResult;

/**
 * <p>
 * 打印机码表 服务类
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-09-30
 */
public interface ConfigPrinterTokenService extends IService<ConfigPrinterToken> {

    /**
     * 磅单打印
     * @param userInfo
     * @param rq
     * @return
     */
    ResponseResult printPoundSheet(AuthPlatformUserInfo userInfo, PrintRq rq);
}
