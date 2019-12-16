package com.bee.platform.common.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @ClassName SystemCodeType
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/5/27$ 14:54$
 * @version 1.0.0
 */

@Getter
@NoArgsConstructor
@AllArgsConstructor
public enum SystemCodeType {
    /**
     * 角色类型
     */
    ROLRTYPE("role_type"),
    /**
     * 子系统
     */
    SUBSYSTEM("sub_system");

    private String code;
}
