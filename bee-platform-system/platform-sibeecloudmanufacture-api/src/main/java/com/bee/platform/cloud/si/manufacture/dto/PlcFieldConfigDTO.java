package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * <p>
 * plc漏斗配置相关数据
 * </p>
 *
 * @author MP123
 * @since 2019-10-11
 */
@Data
@Accessors(chain = true)
@ApiModel("plc漏斗配置相关数据")
public class PlcFieldConfigDTO implements Serializable{

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("字段英文名")
    private String field;

    @ApiModelProperty("字段中文名")
    private String fieldName;

    @ApiModelProperty("plcId")
    private Integer plcId;

    @ApiModelProperty("字段类型")
    private Integer  fieldType;

    @ApiModelProperty("字段类型描述")
    private String  fieldTypeDesc;


    public PlcFieldConfigDTO() {
    }

    public PlcFieldConfigDTO(String field, String fieldName, Integer plcId,
                             Integer fieldType, String fieldTypeDesc) {
        this.field = field;
        this.fieldName = fieldName;
        this.plcId = plcId;
        this.fieldType = fieldType;
        this.fieldTypeDesc = fieldTypeDesc;
    }
}
