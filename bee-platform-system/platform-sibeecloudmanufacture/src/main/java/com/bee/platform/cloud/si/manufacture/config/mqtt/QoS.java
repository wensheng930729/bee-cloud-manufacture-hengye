/**
 * Copyright (C) 2010-2012, FuseSource Corp.  All rights reserved.
 *
 *     http://fusesource.com
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.bee.platform.cloud.si.manufacture.config.mqtt;

/**
 * @notes:
 * @Author: junyang.li
 * @Date: 16:21 2019/10/10
 */
public enum QoS {
    /**
     * mqtt消息等级
     */
    QOS_0(0),
    QOS_1(1),
    QOS_2(2),
    ;

    private int key;

    QoS(int key) {
        this.key = key;
    }

    public int getKey() {
        return key;
    }
}
