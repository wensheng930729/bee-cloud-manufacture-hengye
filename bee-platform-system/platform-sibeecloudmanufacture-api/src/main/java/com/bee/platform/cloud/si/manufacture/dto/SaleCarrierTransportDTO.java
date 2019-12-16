package com.bee.platform.cloud.si.manufacture.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;
import org.springframework.format.annotation.DateTimeFormat;

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

/**
 * @ClassName SaleCarrierTransportDTO
 * @Description 批次运输段承运方信息信息
 * @author qhwang
 * @version 1.0.0
 * @Date 2019/9/27 9:31
 */
@Data
@Accessors(chain = true)
@ApiModel("运输段承运方信息")
public class SaleCarrierTransportDTO {

    @ApiModelProperty("id")
    private Long id;

    @ApiModelProperty("批次ID")
    private String batchId;

    @ApiModelProperty("采购合同业务id")
    private String contractBusinessId;

    @ApiModelProperty("承运方运输段ID")
    private String carrierTransportId;

    @ApiModelProperty("运输段id")
    private String transportSectionId;

    @ApiModelProperty("承运方id")
    private Long carrierId;

    @ApiModelProperty("承运方")
    private String carrierName;

    @ApiModelProperty("运输方式(1-汽车 2-轮船 3-火车)")
    private Integer transportMode;

    @ApiModelProperty("运输方式描述(1-汽车 2-轮船 3-火车)")
    private String transportModeName;

    @ApiModelProperty("是否到厂(0-不到厂 1-到厂)")
    private Integer toFactory;

    @ApiModelProperty("是否到厂描述(0-不到厂 1-到厂)")
    private String toFactoryName;

    @ApiModelProperty("起始地地点id")
    private Integer startingPlaceId;

    @ApiModelProperty("起始地")
    private String startingPlace;

    @ApiModelProperty("到达地地点id")
    private Integer arrivalPlaceId;

    @ApiModelProperty("到达地")
    private String arrivalPlace;

    @ApiModelProperty("运量")
    private BigDecimal freightVolume;

    @ApiModelProperty("单价")
    private BigDecimal unitPrice;

    @ApiModelProperty("运费")
    private BigDecimal carriage;

    @ApiModelProperty("出发时间")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm")
    private Date departureTime;

    @ApiModelProperty("预计到达时间")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm")
    private Date estimateArrivalTime;

    @ApiModelProperty("运输段中的车次信息")
    private List<SaleTransportDetailDTO> detailDTOS;

}
