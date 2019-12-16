package com.bee.platform.common.entity;

import com.bee.platform.common.dto.RegionDTO;
import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * @Classname AuthPlatformUserInfo
 * @Description 用户信息表
 * @Date 2019/5/24 11:14
 * @Author xin.huang
 */
@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain = true)
public class
AuthPlatformUserInfo implements Serializable {

    private static final long serialVersionUID = 1L;
    /**
     *
     */
    private Integer id;
    /**
     * 用户账号
     */
    private String username;
    /**
     * 姓名
     */
    private String name;
    /**
     * 用户名
     */
    @Deprecated
    private String nickname;
    /**
     * 业务编号
     */
    private String beesrvId;
    /**
     * 系统token
     */
    private String sysToken;
    /**
     * 子系统token失效时间
     */
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Date expiresIn;
    /**
     * 当前登录的客户端唯一标识
     */
    private String currentClientId;
    /**
     * 工厂id
     */
    private Integer factoryId;
    /**
     * 工厂名称
     */
    private String factoryName;
    /**
     * 子系统token失效时间秒
     */
    private Integer expiresTime;
    /**
     * 当前登录的企业id
     */
    private Integer orgId;
    /**
     * 企业名称
     */
    private String org_name;
    /**
     * 用户角色
     */
    private AuthRoleInfo roleInfo;
    /**
     * 手机号
     */
    private String phone;
    /**
     * 邮箱
     */
    private String email;
    /**
     * 头像
     */
    private String head;
    /**
     * QQ
     */
    private String qq;
    /**
     * 详细地址
     */
    private String address;
    /**
     * 固话
     */
    private String fixtel;
    /**
     * 地区信息
     */
    private RegionDTO region;
    /**
     * 县级地区id
     */
    private String regionId;
    /**
     * 用户类型：0中台用户 1后台用户 2普通用户
     */
    private Integer userType;
    /**
     * 用户部门
     */
    private String departmentName;
    /**
     * 职位名称
     */
    private String post;
    /**
     * 管理员类型: 0企业成员 1企业管理员 2超级管理员
     */
    private Integer managerType;
}
