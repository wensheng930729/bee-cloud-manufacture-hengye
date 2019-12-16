package com.bee.platform.common.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

/**
 * @notes 角色
 * @Author junyang.li
 * @Date 17:22 2019/3/4
 **/
@Getter
@Setter
@Accessors(chain = true)
@NoArgsConstructor
public class RoleInfo extends Model<RoleInfo> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    private String name;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

    @Override
    public String toString() {
        return "URoles{" +
        ", id=" + id +
        ", name=" + name +
        "}";
    }
}
