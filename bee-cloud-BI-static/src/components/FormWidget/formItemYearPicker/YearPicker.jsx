import React, { Component } from 'react';
import { DatePicker } from 'antd';
import PropTypes from 'prop-types';
import moment from 'moment';

class YearPicker extends Component {
  constructor(props) {
    super(props);
    this.state = {
      isopen: false,
    };
  }

  handlePanelChange = value => {
    const { format, onChange } = this.props;
    this.setState({
      isopen: false,
    });
    if (onChange) {
      onChange(moment(value).format(format));
    }
  };

  handleOpenChange = status => {
    // console.log(status)
    if (status) {
      this.setState({ isopen: true });
    } else {
      this.setState({ isopen: false });
    }
  };

  render() {
    const { format, value } = this.props;

    return (
      <DatePicker
        value={moment(value, format)}
        open={this.state.isopen}
        mode="year"
        placeholder="请选择年份"
        format="YYYY"
        allowClear={false}
        onOpenChange={this.handleOpenChange}
        onPanelChange={this.handlePanelChange}
        onChange={this.clearValue}
      />
    );
  }
}

YearPicker.propTypes = {
  value: PropTypes.string,
  onChange: PropTypes.func,
  format: PropTypes.string,
};

YearPicker.defaultProps = {
  value: moment().format('YYYY'),
  onChange: undefined,
  format: 'YYYY',
};

export default YearPicker;
