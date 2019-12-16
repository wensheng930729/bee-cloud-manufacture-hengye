package com.bee.platform.common.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

/**
 * @ClassName CommonRegion
 * @Description 全国地区表
 * @author qhwang
 * @Date 2018/12/29 9:26
 * @version 1.0.0
 */
@NoArgsConstructor
@Getter
@Setter
@ToString
@Accessors(chain = true)
@TableName("common_region")
public class Region extends Model<Region> {

    private static final long serialVersionUID = 1L;

    /**
     * 主键
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;

    /**
     * 父及地区关系
     */
    private Integer pid;

    /**
     * 地区名称
     */
    private String district;

    /**
     * 子属级别关系
     */
    private Integer level;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
