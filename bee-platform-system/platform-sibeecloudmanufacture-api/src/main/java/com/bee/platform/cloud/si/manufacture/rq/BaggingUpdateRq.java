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
 * @Description 吨袋信息修改入参
 * @author qhwang
 * @version 1.0.0
 * @Date 2019/11/25 16:33
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "吨袋信息修改入参")
public class BaggingUpdateRq implements Serializable {

    private static final long serialVersionUID = 8759706682282429768L;

    @ApiModelProperty("吨袋编号")
    @NotBlank(message = "吨袋编号不能为空")
    private String baggingCode;

    @ApiModelProperty("产品规格id")
    @NotNull(message = "产品规格不能为空")
    private Integer productSpecId;

    @ApiModelProperty("吨袋重量")
    @NotNull(message = "吨袋重量不能为空")
    private BigDecimal productNumber;

    @ApiModelProperty("入库仓库id")
    @NotNull(message = "入库仓库不能为空")
    private Integer storageId;

}
