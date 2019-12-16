package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * <p>
 * 版本升级描述
 * </p>
 *
 * @author LL123
 * @since 2019-11-25
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "升级信息DTO")
public class VersionUpgradeDTO {

    private static final long serialVersionUID = 1L;

    /**
     * 版本号
     */
    private String versionNum;
    /**
     * 描述

     */
    private String description;
    /**
     * 升级时间
     */
    private String upgradeTime;

}
