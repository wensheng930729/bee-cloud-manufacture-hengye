package com.bee.platform.cloud.si.manufacture.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.Date;

/**
 * @ClassName: TonBagDetailDTO
 * @Description: 吨袋详细信息
 * @Author: qhwang
 * @Date: 2019/11/25 16:28
 * @Version: 1.0
 */
@Data
@ApiModel("吨袋详细信息")
@Accessors(chain = true)
public class TonBagDetailDTO implements Serializable {

    private static final long serialVersionUID = 2117920335012040837L;

    @ApiModelProperty("吨袋编号")
    private String tonBagNumber;

    @ApiModelProperty("吨袋重量")
    private BigDecimal productNumber;

    @ApiModelProperty("货物id")
    private Integer productId;

    @ApiModelProperty("样品名称")
    private String productName;

    @ApiModelProperty("货物数量单位")
    private String productUnit;

    @ApiModelProperty("产品规格id")
    private Integer productSpecId;

    @ApiModelProperty("产品规格名称")
    private String productSpecName;

    @ApiModelProperty("入库仓库id")
    private Integer storageId;

    @ApiModelProperty("入库仓库名称")
    private String storageName;

    @ApiModelProperty("炉号")
    private String furnaceNumber;

    @ApiModelProperty("炉次")
    private String furnaceTimes;

    @ApiModelProperty("班次")
    private String scheduling;

    @ApiModelProperty("装袋时间")
    @JsonFormat(pattern = "yyyy年MM月dd日 HH:mm")
    private LocalDateTime createTime;

    @ApiModelProperty("是否可以编辑")
    private Boolean editable;

    @ApiModelProperty("质检信息")
    private DetailBySampleCodeProDTO sampleInfo;

}
