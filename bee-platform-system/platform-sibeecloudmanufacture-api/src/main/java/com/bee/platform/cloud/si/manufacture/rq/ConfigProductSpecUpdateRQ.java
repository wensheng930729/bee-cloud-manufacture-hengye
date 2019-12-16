package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.io.Serializable;
import java.util.List;

/**
 * <p>
 * 产品规格表
 * </p>
 *
 * @author MP123
 * @since 2019-10-15
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("产品规格修改请求参数")
public class ConfigProductSpecUpdateRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("产品规格列表")
    @NotNull(message = "产品规格列表至少一条数据,不能为空")
    @Size(min = 1,message = "产品规格至少一条数据")
    List<ConfigProductSpecRQ> productSpecList;




}
