package com.bee.platform.common.entity;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @author dell
 * @version 1.0.0
 * @ClassName IndustryInfo
 * @Description 功能描述
 * @Date 2019/4/24 17:09
 **/
@Data
@NoArgsConstructor
@Accessors(chain = true)
public class IndustryInfo implements Serializable {

    private static final long serialVersionUID = 1L;

    private Integer id;
    /**
     * 父级行业关系
     */
    private Integer pid;
    /**
     * 行业名称
     */
    private String industry;
    /**
     * 子属级别关系
     */
    private Integer level;
}
