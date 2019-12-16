package com.bee.platform.common.entity;

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

/**
 * 角色资源对应表
 * @author jie.chen123
 * @since 2019-03-18
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@TableName("t_role_resource")
public class RoleResource extends Model<RoleResource> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 角色id
     */
    private Integer roleId;
    /**
     * 资源id
     */
    private Long resourceId;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }
}
