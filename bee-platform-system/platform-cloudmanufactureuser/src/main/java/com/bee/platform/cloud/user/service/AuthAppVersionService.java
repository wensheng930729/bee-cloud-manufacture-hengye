package com.bee.platform.cloud.user.service;

import com.bee.platform.cloud.user.dto.AppVersionDTO;
import com.bee.platform.cloud.user.entity.AuthAppVersion;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.ResponseResult;

import java.util.List;

/**
 * <p>
 *  app最新的版本号
 * </p>
 *
 * @author junyang.li123
 * @since 2019-10-12
 */
public interface AuthAppVersionService extends IService<AuthAppVersion> {

    /**
     * @notes: 获取app最新的版本号
     * @Author: junyang.li
     * @Date: 9:14 2019/9/30
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.cloud.user.dto.AppVersionDTO>
     */
    ResponseResult<List<AppVersionDTO>> getVersion();
    /**
     * @notes: 获得最新的app地址，不包含域名
     * @Author: junyang.li
     * @Date: 18:11 2019/10/24
     * @return: java.lang.String
     */
    String getDownload();
}
