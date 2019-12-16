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
@ApiModel("plc漏斗保存请求参数")
public class PlcFieldConfigSaveRQ implements Serializable{

    private static final long serialVersionUID = 1L;


    @ApiModelProperty("plcId")
    @NotNull(message = "plcId 不能为空")
    @Range(message = "plcId 不能为负")
    private Integer plcId;


    @ApiModelProperty("漏斗id")
    @NotEmpty(message = "漏斗id不能为空")
    @Length(max =40,message = "漏斗id限制40个字符")
    private String field;

    @ApiModelProperty("漏斗名称")
    @NotEmpty(message = "漏斗名称为空")
    @Length(max =50,message = "漏斗名称限制50个字符")
    private String fieldName;

}
