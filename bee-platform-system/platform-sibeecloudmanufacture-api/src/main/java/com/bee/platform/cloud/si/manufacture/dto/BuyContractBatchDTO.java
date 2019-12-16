package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

/**
 * @ClassName BuyContractBatchDTO
 * @Description 合同查询物流批次信息
 * @author qhwang
 * @version 1.0.0
 * @Date 2019/9/24 9:44
 */
@Data
@Accessors(chain = true)
@ApiModel("合同详情中的物流批次信息")
public class BuyContractBatchDTO {

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

    @ApiModelProperty("最早出厂时间")
    private Date departureTime;

    @ApiModelProperty("最早称重时间")
    private Date weightTime;

    @ApiModelProperty("运输方式(汽车、轮船、火车)")
    private String transportMode;

    @ApiModelProperty("批次中车辆化验确认情况")
    private List<BuyCarSampleDTO> carSampleDTOS;

}
