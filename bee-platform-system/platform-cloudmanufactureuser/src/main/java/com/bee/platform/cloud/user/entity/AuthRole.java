package com.bee.platform.cloud.user.entity;

import java.io.Serializable;

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
 * 角色表
 * </p>
 *
 * @author zhigang.zhou123
 * @since 2019-09-19
 */
@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain = true)
public class AuthRole extends Model<AuthRole> {

    private static final long serialVersionUID = 1L;

    /**
     * 角色id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 角色名称
     */
    private String roleName;
    /**
     * super_admin 管理员 。 base 基础角色
     */
    private String roleType;
    /**
     * 企业id
     */
    private Integer enterpriseId;
    /**
     * 角色级别
     */
    private Integer level;
    /**
     * 子系统标识
     */
    private String subSys;
    /**
     * 描述
     */
    private String describe;
    /**
     * 是否删除0未删除1已删除
     */
    private Integer deleted;
    /**
     * 创建人
     */
    private Integer createUser;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 修改时间
     */
    private Date updateTime;
    /**
     * 修改人
     */
    private Integer updateUser;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }
}
