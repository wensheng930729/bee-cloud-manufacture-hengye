package com.bee.platform.cloud.si.manufacture.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * @author xin.huang
 * @description
 * @date 2019/9/25
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "配料返回信息")
public class ProIngredientDTO implements Serializable {
    private static final long serialVersionUID = 5720308416396724872L;

    @ApiModelProperty("配料id")
    private Long id;

    @ApiModelProperty("料批id")
    private Long batchId;

    @ApiModelProperty("料批名称")
    private String materialName;

    @ApiModelProperty("plc设备名称")
    private String plcName;

    @ApiModelProperty("创建时间")
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Date createTime;

    @ApiModelProperty(value = "配料明细返回信息")
    private List<ProIngredientDetailDTO> ingredientDetailList;
}
