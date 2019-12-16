package com.bee.platform.cloud.user.service;

import com.bee.platform.cloud.user.rq.AuthUserRQ;
import com.bee.platform.cloud.user.rq.CodeLoginRQ;
import com.bee.platform.common.constants.enums.PlatformType;
import com.bee.platform.common.entity.AppUserInfo;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;


/**
 * @description:
 * @author: junyang.li
 * @create: 2019-03-04 16:54
 **/
public interface AuthUserLoginService {
    /**
     * @notes 登录
     * @Author junyang.li
     * @Date 16:56 2019/3/4
     **/
    ResponseResult<AuthPlatformUserInfo> login(AuthUserRQ authUserRQ, PlatformType type);
    /**
     * @notes: app端账号密码登录
     * @Author: junyang.li
     * @Date: 20:00 2019/9/24
     * @param authUserRQ :
     * @return: com.bee.platform.common.entity.AppUserInfo
     */
    ResponseResult<AppUserInfo> clientLogin(AuthUserRQ authUserRQ);
    /**
     * @notes: web端短信验证码登录
     * @Author: junyang.li
     * @Date: 14:10 2019/9/25
     * @param rq : 传入参数
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.AuthPlatformUserInfo>
     */
    ResponseResult<AuthPlatformUserInfo> codeLogin(CodeLoginRQ rq);
    /**
     * @notes: app端短信验证码登录
     * @Author: junyang.li
     * @Date: 14:10 2019/9/25
     * @param rq : 传入参数
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.AuthPlatformUserInfo>
     */
    ResponseResult<AppUserInfo> appCodeLogin(CodeLoginRQ rq);
    /**
     * @notes 注销登录
     * @Author junyang.li
     * @Date 9:50 2019/3/5
     **/
    ResponseResult<ResCodeEnum> logout(String sysToken);
}
