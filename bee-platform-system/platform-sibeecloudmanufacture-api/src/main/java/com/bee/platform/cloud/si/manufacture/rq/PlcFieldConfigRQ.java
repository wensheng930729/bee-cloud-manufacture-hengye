package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import lombok.experimental.Accessors;
import org.hibernate.validator.constraints.Length;
import org.hibernate.validator.constraints.Range;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.Date;

/**
 * plc字段相关的配置
 *
 * @author junyang.li
 * @since 2019-10-11
 */
@Data
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("plc字段相关的配置参数")
public class PlcFieldConfigRQ implements Serializable{

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("漏斗id")
    @NotEmpty(message = "漏斗id不能为空")
    @Length(max =40,message = "字段英文名称限制40个字符")
    private String field;

    @ApiModelProperty("漏斗名称")
    @NotEmpty(message = "漏斗名称为空")
    @Length(max =50,message = "字字段中文名称限制50个字符")
    private String fieldName;

    @ApiModelProperty("PLC下料斗类型")
    @NotNull(message = "PLC下料斗类型不能为空")
    @Range(min = 1,max = 2,message = "PLC下料斗类型只能选择批次或者料斗")
    private Integer fieldType;
}
