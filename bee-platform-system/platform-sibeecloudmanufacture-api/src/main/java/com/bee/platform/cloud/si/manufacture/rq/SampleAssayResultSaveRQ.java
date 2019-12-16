package com.bee.platform.cloud.si.manufacture.rq;

import com.bee.platform.cloud.si.manufacture.dto.SampleAssayResultDTO;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.List;

@Data
@Accessors(chain = true)
@ApiModel("样品化验结果保存RQ")
public class SampleAssayResultSaveRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "业务类型1采购2销售3生产")
    @NotNull
    private Integer businessType;

    @ApiModelProperty("样品编号")
    @NotBlank
    private String sampleCode;

    @ApiModelProperty("样品id")
    private Integer productId;

    @ApiModelProperty("化验结果")
    @NotNull
    private List<SampleAssayResultDTO> resultList;
}
