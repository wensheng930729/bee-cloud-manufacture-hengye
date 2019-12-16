package com.bee.platform.cloud.si.manufacture.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.enums.IdType;
import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.Version;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

/**
 * <p>
 * plc硬件通过mqtt传输的实时数据
 * </p>
 *
 * @author MP123
 * @since 2019-10-12
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Accessors(chain = true)
public class PlcRealData extends Model<PlcRealData> {

    private static final long serialVersionUID = 1L;

    /**
     * 主键
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * plc漏斗id
     */
    private String filed;
    /**
     * mqtt中漏斗的值
     */
    private String value;
    /**
     * 工厂id
     */
    private Integer plcId;
    /**
     * 最近更新时间
     */
    private Date time;
    /**
     * 是否有效 0有效， 1 无效
     */
    private Integer status;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }

    public PlcRealData() {
    }

    public PlcRealData(String filed, String value, Integer plcId, Date time, Integer status) {
        this.filed = filed;
        this.value = value;
        this.plcId = plcId;
        this.time = time;
        this.status = status;
    }

    public PlcRealData(String filed, String value, Integer status) {
        this.filed = filed;
        this.value = value;
        this.status = status;
    }
}
