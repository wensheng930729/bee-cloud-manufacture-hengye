package com.bee.platform.cloud.si.manufacture.dto;

import lombok.Data;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * plc硬件通过mqtt传输的实时数据
 * </p>
 *
 * @author MP123
 * @since 2019-10-10
 */
@Data
@ToString
@Accessors(chain = true)
public class PlcRealDataDTO implements Serializable{

    private static final long serialVersionUID = 1L;
    /**
     * 漏斗id
     */
    private String filed;
    /**
     * mqtt 中 漏斗id对应的值
     */
    private String value;
    /**
     * 字段类型
     */
    private Integer filedType;

    public PlcRealDataDTO(String filed, String value) {
        this.filed = filed;
        this.value = value;
    }
}
