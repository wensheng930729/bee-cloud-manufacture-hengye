package com.bee.platform.cloud.user.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @description:
 * @author: junyang.li
 * @create: 2019-09-30 09:09
 **/
@Getter
@Setter
@Accessors(chain = true)
@ToString
@NoArgsConstructor
public class AppVersionDTO implements Serializable {

    private static final long serialVersionUID = 6809074646579901790L;

    @ApiModelProperty("app类型")
    private String appType;

    @ApiModelProperty("app最新版本下载地址")
    private String downloadAddr;

    @ApiModelProperty("app最新版本")
    private String latestVersion;

    @ApiModelProperty("备用最新下载地址")
    private String spareAddr;
}
