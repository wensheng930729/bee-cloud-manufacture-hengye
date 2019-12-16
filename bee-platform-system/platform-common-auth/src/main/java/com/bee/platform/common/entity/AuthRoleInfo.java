package com.bee.platform.common.entity;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @Classname UserRoleInfo
 * @Description 用户角色信息
 * @Date 2019/5/27 13:41
 * @Author xin.huang
 */
@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain = true)
public class AuthRoleInfo implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 角色id
     */
    private Integer roleId;
    /**
     * 角色名称
     */
    private String roleName;
    /**
     * 用户id
     */
    private Integer userId;
    /**
     * 角色类型
     */
    private String roleType;
    /**
     * 角色级别
     */
    private Integer level;
    /**
     * 子系统标识
     */
    private String subSys;

}
