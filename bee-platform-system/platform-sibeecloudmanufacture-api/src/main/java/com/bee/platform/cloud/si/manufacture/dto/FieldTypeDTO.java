package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;

/**
 * @description: PLC下料斗配置类型
 * @author: junyang.li
 * @create: 2019-10-21 11:28
 **/
@Getter
@Setter
@ApiModel("PLC下料斗类型")
public class FieldTypeDTO implements Serializable {


    private static final long serialVersionUID = 7318479118303758368L;

    @ApiModelProperty("类型code")
    private Integer key;

    @ApiModelProperty("类型描述")
    private String value;

    public FieldTypeDTO(Integer key, String value) {
        this.key = key;
        this.value = value;
    }
}
