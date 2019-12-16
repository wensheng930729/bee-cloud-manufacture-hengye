package com.bee.platform.cloud.si.manufacture.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.Version;

import lombok.*;
import lombok.experimental.Accessors;

/**
 * <p>
 * 打印机码表
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-09-30
 */
@NoArgsConstructor
@Setter
@Getter
@Accessors(chain=true)
@ToString
@TableName("config_printer_url")
public class ConfigPrinterUrl extends Model<ConfigPrinterUrl> {


    private static final long serialVersionUID = -5699853828950903649L;
    /**
     * id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * url类型
     */
    private Integer type;
    /**
     * url
     */
    private String url;
    /**
     * 描述
     */
    private String descption;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
