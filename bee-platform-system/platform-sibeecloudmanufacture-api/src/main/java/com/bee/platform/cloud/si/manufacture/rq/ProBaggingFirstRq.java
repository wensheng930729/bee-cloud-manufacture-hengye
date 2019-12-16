package com.bee.platform.cloud.si.manufacture.rq;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
import org.springframework.format.annotation.DateTimeFormat;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
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
@ApiModel(value = "成品装袋保存入参")
public class ProBaggingFirstRq implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("吨袋编号")
    @NotBlank(message = "吨袋编号不能为空")
    private String baggingCode;

    @ApiModelProperty("炉号id")
    @NotNull(message = "炉号id不能为空")
    private Integer furnaceId;

    @ApiModelProperty("炉号名称")
    private String furnaceName;

    @ApiModelProperty("班次编码")
    @NotNull(message = "班次编码不能为空")
    private Integer shiftCode;

    @ApiModelProperty("班次:1一班，2二班，3三班")
    private String shift;

    @ApiModelProperty("班次时间")
    @DateTimeFormat(pattern = "yyyy-MM-dd")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd")
    @NotNull(message = "班次时间不能为空")
    private Date shiftTime;

    @ApiModelProperty("出炉批次编号")
    private Integer furnaceBatchCode;

    @ApiModelProperty("出炉批次")
    private String furnaceBatch;


}
