package com.bee.platform.cloud.si.manufacture.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

/**
 * @author xin.huang
 * @description
 * @date 2019/9/24
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "料批详情返回信息")
public class MaterialBatchDTO implements Serializable {
    private static final long serialVersionUID = -4323070788565084268L;

    @ApiModelProperty("料批id")
    private Long id;

    @ApiModelProperty("料批名称")
    private String materialName;

    @ApiModelProperty("成品id")
    private Integer finishProductId;

    @ApiModelProperty("成品名称")
    private String finishProductName;

    @ApiModelProperty("plc设备id")
    private Integer plcId;

    @ApiModelProperty("plc设备名称")
    private String plcName;

    @ApiModelProperty("生产数量")
    private BigDecimal productionNum;

    @ApiModelProperty("单位")
    private String finishUnit;

    @ApiModelProperty("状态：1启用，0停用")
    private Integer active;

    @ApiModelProperty("创建时间")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date createTime;

    @ApiModelProperty("料批明细返回参数")
    private List<MaterialBatchDetailDTO> materialBatchDetailS;
}
