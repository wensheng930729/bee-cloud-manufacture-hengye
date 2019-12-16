package com.bee.platform.common.entity;

import com.bee.platform.common.dto.RegionDTO;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * @description:
 * @author: junyang.li
 * @create: 2018-12-11 18:15
 **/
@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain = true)
@Deprecated
public class UserInfo implements Serializable {

    private static final long serialVersionUID = 5915421334057673485L;
    /**
     *
     */
    private Integer id;
    /**
     * 用户账号
     */
    private String username;
    /**
     * 用户名
     */
    private String consumerName;
    /**
     * 业务编号
     */
    private String beesrvId;
    /**
     * 用户唯一id
     */
    private String uuid;
    /**
     * 系统token
     */
    private String sysToken;
    /**
     * 子系统token失效时间
     */
    private Date expiresIn;
    /**
     * 子系统token失效时间秒
     */
    private Long expiresTime;
    /**
     * 当前登录的企业id
     */
    private Integer orgId;
    /**
     * 企业名称
     */
    private String org_name;
    /**
     * 用户角色id
     */
    private Integer roleId;
    /**
     * 用户角色name
     */
    private String roleName;
    /**
     * 昵称
     */
    private String nickname;
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
    private String regionid;

    /**
     * 是否是管理员 0成员，1是管理员
     */
    private Integer manage;
    /**
     * 用户部门
     */
    private DepartmentInfo department;
    /**
     * 职位名称
     */
    private String post;
    /**
     * 创建时间
     */
    private Date createAt;
    /**
     * 更新时间
     */
    private Date updateAt;



}
