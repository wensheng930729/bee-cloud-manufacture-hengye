package com.bee.platform.cloud.user.entity;

import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * <p>
 * 登录平台的用户表
 * </p>
 *
 * @author xin.huang
 * @since 2019-05-20
 */
@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain=true)
@TableName("auth_platform_user")
public class AuthPlatformUser extends Model<AuthPlatformUser> {

    private static final long serialVersionUID = 1L;

    /**
     * 用户id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;

    /**
     * 业务id
     */
    private String beesrvId;
    /**
     * 手机号
     */
    private String phone;
    /**
     * 姓名
     */
    private String name;
    /**
     * 用户账号：现在是手机号
     */
    private String username;
    /**
     * 用户名
     */
    private String nickname;
    /**
     * 密码
     */
    private String password;
    /**
     * 头像
     */
    private String head;
    /**
     * 邮箱
     */
    private String email;
    /**
     * qq
     */
    private String qq;
    /**
     * 县级地区id
     */
    private String regionId;
    /**
     * 详细地址
     */
    private String address;
    /**
     * 固话
     */
    private String fixtel;
    /**
     * 登录凭证
     */
    private String sysToken;
    /**
     * token失效时间
     */
    private Date expiresIn;
    /**
     * 当前登录企业id
     */
    private Integer currentEnterpriseId;
    /**
     * 当前登录的客户端唯一标识
     */
    private String currentClientId;
    /**
     * 是否启用：1启用 0禁用
     */
    private Integer status;
    /**
     * 用户激活类型：0平台注册 1平台添加
     */
    private Integer activeType;
    /**
     * 用户类型：0中台用户 1后台用户 2普通用户
     */
    private Integer userType;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 更新时间
     */
    private Date updateTime;
    /**
     * 修改人id
     */
    private Integer updateUser;
    /**
     * 是否删除：1是 0否
     */
    private Integer deleted;

    /**
     * 账号说明
     */
    private String accountDescription;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
