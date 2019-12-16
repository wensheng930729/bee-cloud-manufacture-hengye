package com.bee.platform.cloud.si.manufacture.rq;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
import org.springframework.format.annotation.DateTimeFormat;

import java.io.Serializable;
import java.math.BigDecimal;
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
@ApiModel(value = "成品装袋第二步保存入参")
public class ProBaggingSecondRq implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("吨袋编号")
    private String baggingCode;

    @ApiModelProperty("设备编号")
    private String deviceId;

    @ApiModelProperty("数量")
    private BigDecimal amount;


}
