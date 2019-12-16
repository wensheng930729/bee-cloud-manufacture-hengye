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
 * 资源表
 * </p>
 *
 * @author zhigang.zhou123
 * @since 2019-09-20
 */
@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain = true)
public class AuthResource extends Model<AuthResource> {

    private static final long serialVersionUID = 1L;

    /**
     * 菜单id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 父id
     */
    private Integer pid;
    /**
     * 子系统标识
     */
    private String subSys;
    /**
     * 菜单名称
     */
    private String name;
    /**
     * 菜单类型(0menu 1button)
     */
    private String resourceType;
    /**
     * 菜单图标
     */
    private String icon;
    /**
     * 菜单url
     */
    private String path;
    /**
     * 菜单com破net
     */
    private String component;
    /**
     * 菜单序号
     */
    private Integer orderNum;
    /**
     * 是否隐藏0展开1隐藏
     */
    private Integer isHide;
    /**
     * 菜单所在位置
     */
    private String position;

    private Integer showType;
    /**
     * 是否删除0未删除1已删除
     */
    private Integer deleted;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 修改时间
     */
    private Date updateTime;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
