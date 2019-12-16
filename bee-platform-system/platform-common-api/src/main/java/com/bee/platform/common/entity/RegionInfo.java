package com.bee.platform.common.entity;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @description: 地区信息
 * @author: junyang.li
 * @create: 2019-03-05 18:36
 **/
@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain = true)
public class RegionInfo implements Serializable {

    private static final long serialVersionUID = 1920622510432649293L;
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
}
