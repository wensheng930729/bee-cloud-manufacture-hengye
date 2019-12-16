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
 * @ClassName BuyCarrierInfoDTO
 * @Description 运输段承运方相关信息
 * @author qhwang
 * @version 1.0.0
 * @Date 2019/9/30 9:31
 */
@Data
@Accessors(chain = true)
@ApiModel("运输段承运方相关信息")
public class BuyCarrierInfoDTO {

    @ApiModelProperty("上一运输段情况")
    private BuyTransportSectionDTO lastTransportSectionInfo;

    @ApiModelProperty("当前运输段信息")
    private BuyTransportSectionDTO transportSectionInfo;

    @ApiModelProperty("当前运输段中承运商信息")
    private BuyCarrierTransportDTO carrierTransportDTO;

}
