package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.util.List;

/**
 * @ClassName SaleLogisticsBatchDTO
 * @Description 物流批次信息
 * @author qhwang
 * @version 1.0.0
 * @Date 2019/9/27 9:44
 */
@Data
@Accessors(chain = true)
@ApiModel("物流批次信息")
public class SaleLogisticsBatchDTO {

    @ApiModelProperty("id")
    private Long id;

    @ApiModelProperty("批次ID")
    private String batchId;

    @ApiModelProperty("采购合同业务id")
    private String contractBusinessId;

    @ApiModelProperty("批次名称")
    private String batchName;

    @ApiModelProperty("运量")
    private BigDecimal freightVolume;

    @ApiModelProperty("到货量")
    private BigDecimal arrivalVolume;

    @ApiModelProperty("批次状态(1-待确认)")
    private Integer batchStatus;

    @ApiModelProperty("物流批次运输段信息")
    private List<SaleTransportSectionDTO> transportSectionDTOS;

}
