package com.bee.platform.common.entity;

import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * @description: app端返回的用户信息
 * @author: junyang.li
 * @create: 2019-09-24 19:35
 **/
@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain = true)
public class AppUserInfo implements Serializable {

    private static final long serialVersionUID = -8731285874573975191L;

    @ApiModelProperty("用户信息")
    private AuthPlatformUserInfo userInfo;

    @ApiModelProperty("当前用户菜单信息")
    private List<AuthResourceInfo> resourceInfo;

    public AppUserInfo(AuthPlatformUserInfo userInfo, List<AuthResourceInfo> resourceInfo) {
        this.userInfo = userInfo;
        this.resourceInfo = resourceInfo;
    }
}
