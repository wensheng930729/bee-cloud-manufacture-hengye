package com.bee.platform.cloud.si.manufacture.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

/**
 * @author xin.huang
 * @description
 * @date 2019/9/25
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "人工补料返回信息")
public class ProArtificialFeedDTO implements Serializable {
    private static final long serialVersionUID = -2089470862001849856L;

    @ApiModelProperty("id")
    private Long id;

    @ApiModelProperty("炉号id")
    private Integer furnaceId;

    @ApiModelProperty("炉号名称")
    private String furnaceName;

    @ApiModelProperty("产品id")
    private Integer productId;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("产品规格id")
    private Integer productSpecId;

    @ApiModelProperty("产品规格名称")
    private String productSpecName;

    @ApiModelProperty("产品单位")
    private String unit;

    @ApiModelProperty("班次:1一班，2二班，3三班")
    private Integer shift;

    @ApiModelProperty("产品数量")
    private BigDecimal num;

    @ApiModelProperty("仓库id")
    private Integer warehouseId;

    @ApiModelProperty("仓库名称")
    private String warehouseName;

    @ApiModelProperty("添加原因")
    private String addReason;

    @ApiModelProperty("下料时间")
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm")
    private Date blankingTime;

    @ApiModelProperty("创建时间")
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Date createTime;

    @ApiModelProperty(value = "当前日期")
    private String currentDate;

    @ApiModelProperty("成品id")
    private Integer finishProductId;

    @ApiModelProperty("成品名称")
    private String finishProductName;
}
