package com.bee.platform.cloud.si.manufacture.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
import org.springframework.format.annotation.DateTimeFormat;

import java.io.Serializable;
import java.util.Date;

/**
 * @author chenxm66777123
 * @version 1.0.0
 * @Description
 * @Date 2019/10/9 9:53
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "成品装袋第一步保存出参")
public class ProBaggingFirstDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("吨袋编号")
    private String baggingCode;

    @ApiModelProperty("炉号id")
    private Integer furnaceId;

    @ApiModelProperty("炉号名称")
    private String furnaceName;

    @ApiModelProperty("班次编码")
    private Integer shiftCode;

    @ApiModelProperty("班次:1一班，2二班，3三班")
    private String shift;

    @ApiModelProperty("班次时间")
    @DateTimeFormat(pattern = "yyyy-MM-dd")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd")
    private Date shiftTime;

    @ApiModelProperty("出炉批次编号")
    private Integer furnaceBatchCode;

    @ApiModelProperty("出炉批次")
    private String furnaceBatch;

    @ApiModelProperty("产品id")
    private Integer productId;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("产品规格id")
    private Integer productSpecId;

    @ApiModelProperty("产品规格名称")
    private String productSpecName;
}
