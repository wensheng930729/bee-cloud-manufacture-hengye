package com.bee.platform.cloud.user.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 用户与角色/功能/应用的关联表
 * </p>
 *
 * @author zhigang.zhou123
 * @since 2019-09-19
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@TableName("auth_user_role")
public class AuthUserRole extends Model<AuthUserRole> {

    private static final long serialVersionUID = 1L;

    /**
     * 主键id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 用户id
     */
    private Integer userId;
    /**
     * 角色id
     */
    private Integer roleId;
    /**
     * 企业id
     */
    private Integer enterpriseId;
    /**
     * 角色父id
     */
    private Integer pid;
    /**
     * 是否启用：1启用，0禁用
     */
    private Integer status;
    /**
     * 该用户下角色级别：与角色表的level对应
     */
    private Integer level;
    /**
     * 该用户下角色或功能的自定义分类，基于level字段的一个分类，用于分类展示
     */
    private String roleType;
    /**
     * 该用户下角色的序号
     */
    private Integer orderNum;
    /**
     * 创建人id
     */
    private Integer createUser;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 更新时间
     */
    private Date updateTime;
    /**
     * 逻辑删除，1-是，0-否
     */
    private Integer deleted;
    /**
     * 标识 1 勾选 0未勾选
     */
    private Integer flag;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

    @Override
    public String toString() {
        return "AuthUserRole{" +
        ", id=" + id +
        ", userId=" + userId +
        ", roleId=" + roleId +
        ", enterpriseId=" + enterpriseId +
        ", pid=" + pid +
        ", status=" + status +
        ", level=" + level +
        ", roleType=" + roleType +
        ", orderNum=" + orderNum +
        ", createUser=" + createUser +
        ", createTime=" + createTime +
        ", updateTime=" + updateTime +
        ", deleted=" + deleted +
        ", flag=" + flag +
        "}";
    }
}
