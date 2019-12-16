package com.bee.platform.common.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

/**
 * <p>
 * 字典表
 * </p>
 *
 * @author qhwang123
 * @since 2019-03-21
 */
@NoArgsConstructor
@Getter
@Setter
@Accessors(chain = true)
@TableName("t_dictionary")
public class Dictionary extends Model<Dictionary> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 父级关系
     */
    private Integer pid;
    /**
     * 名称
     */
    private String name;
    /**
     * 字典类型 1行业
     */
    private Integer type;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

    @Override
    public String toString() {
        return "TDictionary{" +
        ", id=" + id +
        ", pid=" + pid +
        ", name=" + name +
        ", type=" + type +
        "}";
    }
}
