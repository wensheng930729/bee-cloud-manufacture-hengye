package com.bee.platform.cloud.user.entity;

import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import lombok.*;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * <p>
 * 企业与角色（角色或功能的关联表）的中间表
 * </p>
 *
 * @author LILIANG
 * @since 2019-05-20
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@AllArgsConstructor
public class AuthEnterpriseRole extends Model<AuthEnterpriseRole> {

    private static final long serialVersionUID = 1L;

    /**
     * 主键id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 企业id
     */
    private Integer enterpriseId;
    /**
     * 角色id
     */
    private Integer roleId;
    /**
     * 父角色id
     */
    private Integer pid;
    /**
     * 该用户下角色级别：与角色表的level对应
     */
    private Integer level;
    /**
     * 该用户下角色或功能的自定义分类。基于level字段的一个分类，用于分类展示
     */
    private String roleType;
    /**
     * 该用户下角色的序号
     */
    private Integer orderNum;
    /**
     * 状态：1启动 0禁用
     */
    private Integer status;
    /**
     * 是否删除 0未删除 1删除
     */
    private Integer deleted;
    /**
     * 标识 1勾选 0未勾选
     */
    private Integer flag;
    /**
     * 创建人
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

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
