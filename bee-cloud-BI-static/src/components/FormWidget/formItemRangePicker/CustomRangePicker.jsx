import React, { Component } from 'react';
import PropTypes from 'prop-types';
import { DatePicker } from 'antd';
import moment from 'moment';

class CustomRangePicker extends Component {
  static getDerivedStateFromProps(nextProps, prevState) {
    const bool1 = nextProps.value && nextProps.value.startTime && nextProps.value.endTime;
    const bool2 =
      nextProps.value.startTime !== prevState.startTime ||
      nextProps.value.endTime !== prevState.endTime;
    if (bool1 && bool2) {
      return {
        startTime: nextProps.value.startTime,
        endTime: nextProps.value.endTime,
      };
    }
    return null;
  }

  state = {
    startTime: null,
    endTime: null,
    endOpen: false,
  };

  disabledStartDate = startTime => {
    const { endTime } = this.state;
    if (!startTime || !endTime) {
      return false;
    }
    return startTime.valueOf() > endTime.valueOf();
  };

  disabledEndDate = endTime => {
    const { startTime } = this.state;
    if (!endTime || !startTime) {
      return false;
    }
    return endTime.valueOf() <= startTime.valueOf();
  };

  onChange = (field, value) => {
    const { onChange, value: propsValue } = this.props;
    const { startTime, endTime } = this.state;
    if (!propsValue) {
      this.setState({
        [field]: value,
      });
    }
    const result = { startTime, endTime, [field]: value };
    if (onChange) {
      onChange(result);
    }
  };

  onStartChange = value => {
    this.onChange('startTime', value);
  };

  onEndChange = value => {
    this.onChange('endTime', value);
  };

  handleStartOpenChange = open => {
    if (!open) {
      this.setState({ endOpen: true });
    }
  };

  handleEndOpenChange = open => {
    this.setState({ endOpen: open });
  };

  render() {
    const {
      format,
      showTime,
      //  value, onChange, ...rest
    } = this.props;

    const { startTime, endTime, endOpen } = this.state;

    return (
      <>
        <DatePicker
          disabledDate={this.disabledStartDate}
          showTime={showTime}
          format={format}
          allowClear={false}
          value={moment(startTime, format)}
          placeholder="开始时间"
          onChange={val => this.onStartChange(moment(val).format(format))}
          onOpenChange={this.handleStartOpenChange}
        />
        ~
        <DatePicker
          disabledDate={this.disabledEndDate}
          showTime={showTime}
          format={format}
          allowClear={false}
          value={moment(endTime, format)}
          placeholder="结束时间"
          onChange={val => this.onEndChange(moment(val).format(format))}
          open={endOpen}
          onOpenChange={this.handleEndOpenChange}
        />
      </>
    );
  }
}

CustomRangePicker.propTypes = {
  showTime: PropTypes.bool,
  value: PropTypes.object,
  onChange: PropTypes.func,
  format: PropTypes.string,
};

CustomRangePicker.defaultProps = {
  showTime: true,
  // value: {
  //   startTime: moment()
  //     .subtract(30, 'days')
  //     .format('YYYY-MM-DD'),
  //   endTime: moment().format('YYYY-MM-DD'),
  // },
  onChange: undefined,
  // YYYY-MM-DD HH:mm:ss
  format: 'YYYY-MM-DD',
};

export default CustomRangePicker;
