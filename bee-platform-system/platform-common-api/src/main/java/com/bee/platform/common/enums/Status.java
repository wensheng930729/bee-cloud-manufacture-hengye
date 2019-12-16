package com.bee.platform.common.enums;

import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @description:
 * @author: junyang.li
 * @create: 2019-03-06 11:33
 **/
@Getter
@NoArgsConstructor
public enum  Status {
    /**
     * false
     */
    FALSE(0),
    /**
     * true
     */
    TRUE(1);

    private Integer key;

    Status(Integer key){
        this.key=key;
    }

    public  static  Status getStatus(Integer key){
        if(key==null){
            return null;
        }
        Status[] item=Status.values();
        for (Status obj:item) {
            if(obj.getKey().equals(key)){
                return obj;
            }
        }
        return null;
    }
    /**
     * @notes: 根据key返回布尔值
     * @Author: junyang.li
     * @Date: 9:56 2019/5/17
     * @param key :
     * @return: java.lang.Boolean
     */
    public static Boolean checkStatus(Integer key){
        if(key==null){
            return false;
        }else if(FALSE.getKey().equals(key)){
            return false;
        }else if(TRUE.getKey().equals(key)){
            return true;
        }else {
            return false;
        }
    }
}